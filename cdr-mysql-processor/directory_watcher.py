import pyinotify
import json
import shutil
import logging
from os import listdir
from os.path import isfile, join
import asyncore

class NewCDRHandler(pyinotify.ProcessEvent):
    """
    Handles processing of the newly created JSON CDR files
    """
    def __init__(self, handle_create, success_dir, failed_dir):
        self.handle_create = handle_create
        self.success_dir = success_dir
        self.failed_dir = failed_dir
        self.logger = logging.getLogger('newCDRHandler')

    def process_IN_CREATE(self, event):
        """
        Method is called when a CDR file is created
        """
        self.logger.debug("New CDR file created: %s" % event.pathname)
        file_name = event.pathname
        self.processCDR(file_name)

    def process_IN_MOVED_TO(self, event):
        """
        Method is called when a CDR file is moved to the directory
        """
        self.logger.debug("CDR file moved to scan directory: %s" % event.pathname)
        file_name = event.pathname
        self.processCDR(file_name)

    def processCDR(self, file_name):
        """
        Call the CDR processor, and possibly move the file
        """
        self.logger.info("Processing CDR: %s" % file_name)

        dest_dir = self.failed_dir
        try:
            with open(file_name) as f:
                j = json.load(f)
                self.handle_create(j)
                dest_dir = self.success_dir
        except ValueError, e:
            self.logger.warning("Failed to decode JSON in file %s: %s" % (file_name, str(e)))
        except Exception, e:
            self.logger.exception("Error processing file %s: %s" % (file_name, str(e)))
        if dest_dir is not None:
            try:
                self.logger.info("Moving file %s to %s" % (file_name, dest_dir))
                shutil.move(file_name, dest_dir)
            except shutil.Error:
                self.logger.warning("Failed to move file %s to %s" % (file_name, dest_dir))

class WatchCDRs():
    """
    Watches for newly created CDR files, and invokes process_IN_CREATE on
    NewCDRHandler.
    """
    def __init__(self, monitor_dir, success_dir=None, failed_dir=None, cdr_processor=None):
        self.success_dir = success_dir
        self.failed_dir = failed_dir
        self.watch_dir = monitor_dir
        self.cdr_processor = cdr_processor

        self.logger = logging.getLogger('newCDRHandler')

        mask = pyinotify.IN_CREATE | pyinotify.IN_MOVED_TO
        self.wm = pyinotify.WatchManager()
        self.wm.add_watch(self.watch_dir, mask, rec=True)
        self.handler = NewCDRHandler(self.cdr_processor, self.success_dir, self.failed_dir)

    def process_existing(self):
        """
        Process files already existing in the directory. This may miss files
        created during method execution
        """
        path = self.watch_dir

        self.logger.info("Searching %s for existing CDRs" % path)
        files_in_dir = [join(path, f) for f in listdir(path) if isfile(join(path, f))]
        for f in files_in_dir:
            self.handler.processCDR(f)
        self.logger.info("Finished searching %s for existing CDRs" % path)

    def watch(self):
        """
        Block the current thread and start processing incomming files
        """
        self.logger.info("Watching %s for incomming CDRs" % self.watch_dir)
        notifier = pyinotify.AsyncNotifier(self.wm, self.handler)
        asyncore.loop()
