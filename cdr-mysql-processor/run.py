#!/usr/bin/env python2
import mysql_process_cdr
import directory_watcher
import config
import logging

def main():
    logging.basicConfig(level=config.LOG_LEVEL, filename=config.LOG_FILE)
    sql_gen = mysql_process_cdr.SQLGenerator()
    watch = directory_watcher.WatchCDRs(config.MONITOR_DIR, success_dir=config.SUCCESS_DIR, failed_dir=config.FAILED_DIR, cdr_processor=sql_gen.process_cdr)
    if config.PROCESS_EXISTING:
        watch.process_existing()
    #Block the current thread
    watch.watch()

if __name__ == '__main__':
    main()
