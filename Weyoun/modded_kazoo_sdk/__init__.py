import kazoo
from kazoo.request_objects import KazooRequest as _KazooRequest
from kazoo.client import Client as _OriginalClient

# Lots of monkey patches
# Basically just allows one to set always_add_headers that has headers added to kazoo requests
# This is to support IDP traversal if needed




# If always_add_headers exists on the client, pass it to the requests just before execute
_original_client_execute = _OriginalClient._execute_request
def _patched_client_execute(self, request, **kwargs):
    extra = getattr(self, "always_add_headers", None)
    if extra is not None:
        kwargs["always_add_headers"] = extra

    return _original_client_execute(self, request, **kwargs)
_OriginalClient._execute_request = _patched_client_execute

# If always_add_headers is passed in the execute, apply it as an attribute to self
_original_execute = _KazooRequest.execute
def _patched_execute(self, base_url, method=None, data=None, token=None,
                     files=None, **kwargs):
    extra = kwargs.pop("always_add_headers", None)
    if extra is not None:
        self.always_add_headers = extra
    return _original_execute(self, base_url, method, data, token, files, **kwargs)
_KazooRequest.execute = _patched_execute

# When collecting headers, add always_add_headers attribute if it exists on the request object
_original_get_headers = _KazooRequest._get_headers
def _patched_get_headers(self, token=None):
    headers = _original_get_headers(self, token)
    extra = getattr(self, "always_add_headers", None)
    if isinstance(extra, dict):
        headers.update(extra)
    return headers
_KazooRequest._get_headers = _patched_get_headers

from kazoo import *
