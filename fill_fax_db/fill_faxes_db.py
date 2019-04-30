#!/usr/bin/env python3

import sys
import requests
import json
import datetime
import random
import couchdb


class generate:
    fixture = None

    json_file = "fixture.json"
    attachments = [
        "fax_file.tiff",
        "original_file.pdf",
        "pdf_file.pdf"
    ]
    database = 'faxes'
    accounts = [
        '61984198ba1179bea57d799b134ef3f5',
        '00000000000000000000000000000000'
    ]

    tx_reasons = [
        'failure',
        'no route found',
        'success',
        'The call dropped prematurely'
    ]

    statuses = [
        'completed',
        'attaching_files',
        'failed',
        'processing',
        'resubmitting',
    ]

    folder = [
        None,
        'inbox',
        'outbox'
    ]

    gregorian_offset = 62167219200

    database_inst = couchdb.Server('http://admin:admin@127.0.0.1:5984')
    db = database_inst[database]

    start = datetime.datetime.strptime('1/1/2019 1:30 PM', '%m/%d/%Y %I:%M %p')
    end = datetime.datetime.now()

    def random_date(self):
        delta = self.end - self.start
        int_delta = (delta.days * 24 * 60 * 60) + delta.seconds
        random_second = random.randrange(int_delta)
        datevalue = self.start + datetime.timedelta(seconds=random_second)
        return int(datevalue.strftime('%s')) + self.gregorian_offset

    def get_fixture(self):
        if not self.fixture:
            f = open(self.json_file)
            fixture = json.loads(f.read())
        return self.randomize(fixture)

    def randomize(self, data):
        data['pvt_modified'] = self.random_date()
        data['tx_result']['result_text'] = random.choice(self.tx_reasons)
        data['pvt_status'] = random.choice(self.statuses)
        data['pvt_job_status'] = data['pvt_status']
        data['pvt_account_id'] = random.choice(self.accounts)
        data['pvt_reseller_id'] = data['pvt_account_id']
        data['folder'] = random.choice(self.folder)
        return data

    def save_doc(self, doc):
        print("saving doc")
        print(doc)
        doc_id, doc_rev = self.db.save(doc)
        return self.db[doc_id]

    def attach(self, doc):
        myrange = random.randint(0,4)
        docid = doc.id
        for i in range(myrange):
            attachme = random.choice(self.attachments)
            print("attaching")
            print(attachme)
            name = random.choice(self.attachments)
            content = open(name, 'r', encoding="ISO8859-1").read()
            self.db.put_attachment(doc, content.encode("utf8"), filename=name)
            doc = self.db[docid]

    def run(self, count):
        for i in range(count):
            doc = self.get_fixture()
            docid = self.save_doc(doc)
            self.attach(docid)


if __name__ == '__main__':
    G = generate()
    G.run(int(sys.argv[1]))
