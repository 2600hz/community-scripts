/**
 * Module dependencies.
 */

var express = require('express');
var http = require('http');
var path = require('path');
var crypto = require('crypto');
var clientio = require('socket.io-client');
var crypto = require('crypto');
var Crossbar = require('crossbar');

var app = express();
var config = require('./config');

// all environments
app.set('port', process.env.PORT || 3000);

http.createServer(app).listen(app.get('port'), function(){
    console.log('blackhole client started ...');

    var socket = clientio.connect(config.blackhole.host , { port: config.blackhole.port });
    var cb_client = new Crossbar({
        'url': config.crossbar.host,
        'port': config.crossbar.port,
        'validate': true
    });

    var clear_creds = config.crossbar.username + ":" + config.crossbar.password;
    var hash_creds = crypto.createHash('md5').update(clear_creds).digest("hex");

    cb_client.api.user_auth.put({
        'data': {
            'credentials': hash_creds,
            'account_name': config.crossbar.account_name
        }
    }, function(err, data) {
        var account_id = data.account_id;
        var auth_token = cb_client.token;

        socket.emit("subscribe", { account_id: account_id, auth_token: auth_token, binding: "call.CHANNEL_CREATE.*"});
        socket.emit("subscribe", { account_id: account_id, auth_token: auth_token, binding: "call.CHANNEL_ANSWER.*"});
        socket.emit("subscribe", { account_id: account_id, auth_token: auth_token, binding: "call.CHANNEL_DESTROY.*"});
        socket.emit("subscribe", { account_id: account_id, auth_token: auth_token, binding: "conference.event.*"});
        socket.on("participants_event", function (data) {
            console.log(data);
        });
        socket.on("CHANNEL_CREATE", function (data) {
            console.log(data);
        });
        socket.on("CHANNEL_ANSWER", function (data) {
            console.log(data);
        });
        socket.on("CHANNEL_DESTROY", function (data) {
            console.log(data);
        });
    });
});
