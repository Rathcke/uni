from http.client import HTTPSConnection
import urllib
import re

host = "forums.oneplus.net"
path = "/forums/dansk-invites/"
params = urllib.parse.urlencode({})
headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
conn = HTTPSConnection(host)
conn.request("GET", path, params, headers)
response = conn.getresponse()
msg = response.read()
msg = msg.decode("utf-8")
matches = re.findall("<a href=\"threads\/.*>(.*)<\/a>", msg)
for match in matches:
  try:
    tmp = int(match)
  except ValueError:
    print(match)
