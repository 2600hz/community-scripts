#!/usr/bin/ruby 
# Script to listen for incoming CDRs from the RabbitMQ server, store the
# CDR in a sqlite database, and send radius account start and stop packets
# for billing


require 'amqp'
require 'json' 
require 'sqlite3'

rabbithost = 'localhost'
rabbituser = 'mycdr'
rabbitpassword = 'mycdr'
rabbitqueue = 'mycdr'

def cmd(str)
	puts str
	system str
end

radius_ip = "192.168.100.100"
rad_secret = "something"

cmd "sqlite3 cdr.db \"create table cdr (billing_second integer not null default 0, duration_second integer not null default 0, call_direction text , timestamp integer ,callee_number text not null, caller_number text not null, hangup_cause text not null, call_id text not null);\" " if Dir["cdr.db"].length == 0

db = SQLite3::Database.new("cdr.db")
stmt = db.prepare "insert into cdr (billing_second,duration_second,call_direction,timestamp,callee_number,caller_number,hangup_cause,call_id) values (?,?,?,?,?,?,?,?);"

id1 = rand(10000)
id2 = rand(10000)

AMQP.start(:host => rabbithost,:user=>rabbituser, :password=>rabbitpassword) do |connection|
	channel = AMQP::Channel.new(connection)
	exchange = channel.topic("callevt")
	queue    = channel.queue(rabbitqueue)
	queue.bind(exchange,:routing_key=>"call.cdr.*")
	Signal.trap("INT") do
    		connection.close do
			stmt.close
			db.close
      			EM.stop { exit }
    		end
  	end
	puts " [*] Waiting for messages. To exit press CTRL+C"
	queue.subscribe do |body|
		j = JSON.parse(body)
		#p j

		begin
			stmt.execute(j['Billing-Seconds'] || 0,j['Duration-Seconds'] || 0 ,j['Call-Direction'],j['Timestamp'],j['Callee-ID-Number'] || j['To-Uri'],j['Caller-ID-Number'], j['Hangup-Cause'],j['Call-ID'])
		rescue
		end
		puts "Call: #{j['Call-Direction']} From:#{j['From-Uri']} To:#{j['To-Uri']} #{j['Billing-Seconds']}"
		next unless j['Billing-Seconds'].to_i > 0
		if j['Call-Direction'] == 'inbound'
			#j.each_pair{ |k,v| puts "#{k} #{v}" }
			user = j['From-Uri'].split('@')[1]
			from_uri = j['From-Uri'] || j['Called-ID-Number']
			called = j['To-Uri'].split('@')[0]
			res = "User-Name = \"#{user}\"\nNAS-IP-Address = \"119.82.248.238\"\nNAS-Port-Id = 12\nCalled-Station-Id = \"#{called}\"\nAcct-Status-Type = Start\nAcct-Session-Id = \"#{sprintf("%x",id1)}\"\nAcct-Unique-Session-Id = \"#{sprintf("%x",id2)}\"\nCalling-Station-Id = \"#{from_uri}\"\nNAS-Port-Type = \"Async\"\n"
		 	puts res
			IO.popen("radclient -x #{radius_ip} acct #{rad_secret}","w") {|proc|
                		proc.write(res)
        		}
			res = "User-Name = \"#{user}\"\nAcct-Session-Time = #{j['Billing-Seconds'] || j['Duration-Seconds'] || 0}\nNAS-IP-Address = \"119.82.248.238\"\nNAS-Port-Id = 12\nCalled-Station-Id = \"#{called}\"\nAcct-Status-Type = Stop\nAcct-Session-Id = \"#{sprintf("%x",id1)}\"\nAcct-Unique-Session-Id = \"#{sprintf("%x",id2)}\"\nCalling-Station-Id = \"#{from_uri}\"\nNAS-Port-Type = \"Async\"\n"
		 	puts res
			IO.popen("radclient -x #{radius_ip} acct #{rad_secret}","w") {|proc|
                 		proc.write(res)
        		}
			id1 += 1
			id2 += 1
		end
	end
end
