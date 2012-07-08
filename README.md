YASA: YET ANOTHER STATS AGGREGATOR
==================================


HTTP API
--------
To increment a counter:

	/incr?key="keyname"&value=<integer_value>

To set a gauge:

	/set?key="keyname"&value=<integer_value>

To get for a key:

	/get?key="keyname"&range=-<integer_value><hour|min|sec|day|month|year>


WebSocket API
-------------

To register to the feed of a key. Returns data for the given range up on registration and
server keeps sending new data periodically.

	{method: "register", key: "keyname", range: "-<integer_value><hour|min|sec|day|month|year>"}

Unregister from a registered feed.

	{method: "unregister", key: "keyname"}

To increment a counter:

	{method: "incr", key: "keyname", value: <integer_value> }

To set a gauge:

	{method: "set", key: "keyname", value: <integer_value> }

Possible Responses:
	1) error: invalid request
		if the request is invalid like registering to a non existing key.
	2) ok
		upon set/incr/unregister operations
	3) pong
		upon ping
	4) [{"key":"keyname", "values":[[timestamp, value], [timestamp, value], ...]}, ...]
		on register and periodic updates


Configuration
-------------

	[
		{yasa,[
			{port, 8080},
			{retentions, [{10,1080},{40,900}]}
		]}
	].

Port is the port of the server, defaults to 8080 if not defined.
Retentions are sampling rate in terms of sample once every X seconds and how many samples you want at that rate. 
The above example translates in to 1080 10 second samples which stores the data for last 3 hours (3 hours = 10800 seconds) and 400 samples at 40 second interval which stores last 9 hours. You can define as many retentions as you want as long as they are divisable by the smallest retention. (you can't have [{5,X}, {10,Y}, {27,Z}]).