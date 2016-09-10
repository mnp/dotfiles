#!/usr/bin/python

import SimpleHTTPServer
import SocketServer
import uuid
import re
import os.path
import requests

PORT = 80
DIR  = "keys"

r = requests.get('http://169.254.169.254/2009-04-04/meta-data/public-ipv4')
BASE = "http://" + r.text

def filename(key):
    return DIR + "/" + key    

class MyRequestHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):

    def log(self, mode, key, msg):
        with open(filename(key), mode) as f:
            f.write(self.log_date_time_string() + " " + msg + "\n")

    def respond(self, code, str):
        self.protocol_version='HTTP/1.1'
        self.send_response(code)
        self.send_header('Content-type', 'text/html')
        self.end_headers()
        self.wfile.write(str + "\r\n")
            
    def do_GET(self):            
        self.server_version = "vanilla/017"
        self.sys_version = "potty/492"

        if self.path == "/new": 	#  optional comment here
            key = uuid.uuid4().hex
            self.log("w", key, "create")
            return self.respond(200, "%s:%d/key/%s" % (BASE, PORT, key))
        
        m = re.match('/delete/([a-f0-9]+)$', self.path)
        if m != None:
            key = m.group(1)            
            try:
                os.remove(filename(key))
            except OSError:
                return self.respond(404, "key not found")
            return self.respond(200, "goodbye")

        m = re.match('/key/([a-f0-9]+)$', self.path)
        if m != None:
            key = m.group(1)
            if os.path.isfile(filename(key)) != True:
                return self.respond(404, "key not found")

            self.log("a", key, "access " + str(self.client_address) + " " + self.command)
            return self.respond(200, "hello")

        ## report

        return self.respond(404, "bad command")

httpd = SocketServer.TCPServer(("", PORT), MyRequestHandler)
print "serving at port", PORT
httpd.serve_forever()
