import config

import logging
import pymysql
import numbers
import datetime

class MySQLClient():
    def __init__(self):
        self.logger = logging.getLogger('MySQLClient')
        self.connect(config.DB_HOST, config.DB_PORT, config.DB_NAME, config.DB_USER, config.DB_PASSWORD)

    def connect(self, host, port, db_name, user, password):
        self.logger.info("Connecting to MySQL database")
        self.connection = pymysql.connect(host=host, port=port, db=db_name, user=user, passwd=password)

    def insert_sql(self, sql):
        cursor = self.connection.cursor()
        self.logger.debug("About to insert SQL: %s" % sql)
        cursor.execute(sql)
        cursor.close()

    def escape(self, string):
        return self.connection.escape(string)

class SQLGenerator():
    def __init__(self):
        self.sql_client = MySQLClient()
        self.logger = logging.getLogger('SQLGenerator')

    def process_cdr(self, cdr):
        self.logger.debug("Processing CDR: %s" % cdr)
        sql = self.generate(cdr)
        self.logger.debug("Generated SQL: %s" % sql)
        self.insert(sql)

    def generate(self, obj):
        db_dict = {}
        FIELDS = config.FIELDS

        def get_json_field(key, dct, default=None):
            if dct is None:
                return default
            key_path = key.split(".", 1)
            if len(key_path) == 1:
                return dct.get(key, default)
            else:
                key, remaining_path = key_path
                return get_json_field(remaining_path, dct.get(key))

        #Create dictionary of database columns and their values
        for json_name, db_name, type_fun in FIELDS:
            field = get_json_field(json_name, obj)
            if isinstance(field, basestring):
                field = field.strip()
            if type_fun is not None:
                field = type_fun(field)
            db_dict[db_name] = field

        #Actually construct insert
        sql  = "INSERT INTO calls (\n" + ", \n".join(["`%s`" % db_name for json_name, db_name, _ in FIELDS]) + ") \n"
        values = []
        for json_name, db_name, _ in FIELDS:
            field = db_dict.get(db_name)
            if field is None:
                values.append("NULL")
            elif isinstance(field, numbers.Number):
                values.append(str(field))
            elif isinstance(field, datetime.datetime):
                values.append(field.strftime("TIMESTAMP '%Y-%m-%d %H:%M:%S'"))
            else:
                values.append(self.sql_client.escape(field))
        sql  += "VALUES (\n" + ", \n".join(values) + ")"
        return sql

    def insert(self, sql):
        self.sql_client.insert_sql(sql)
