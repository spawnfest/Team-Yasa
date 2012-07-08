Yasa
====

YASA is a high performance stat aggregation daemon which collects numeric time-series data based on given keys.
Data is then aggregated and periodically dumped to individual Round Robin Databases depending on its key.
Since YASA is able to load recently touched RRD archives into state its able to significantly reduce its I/O footprint
and quickly service requests without reading/writing to disk.
Each RRD can be queried via Yasa's WebSocket or HTTP API allowing clients to steam/update data.

Currently Yasa only supports two data types for logging data. Gauges and Counters. Gauges are used for
storing things like temperature, memory, eg. a value at a specific point in time. Counters are used for
calculating things like bandwidth, requests/second ie. some kind of rate. (see API)

Configuration
=============

Since RRD files are fixed in sized you must define how much historical data you want to keep lying around.
Yasa will default to 60 samples of 1 second data and 1000 samples of 1 minute data. You can define as
many retentions as you like. For example, in your app.config:

	[
		{yasa,[
      {port, 8080}
			{retentions, [{1,60},{60,1000}]}
		]}
	].

API Examples
============

HTTP API
--------

To set a gauge:

	/api/set?key="keyname"&value=<integer_value>

To increment a counter:

	/api/incr?key="keyname"&value=<integer_value>

To get for a key:

	/api/get?key="keyname"&range=-<integer_value><hour|min|sec|day|month|year>


WebSocket API
-------------

Connect to websocket on /wsapi then;

To register to the feed of a key. Returns data for the given range up on registration and
server keeps sending new data periodically.

	{method: "register", key: "keyname", range: "-<integer_value><hour|min|sec|day|month|year>"}

To unregister from a registered feed.

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
