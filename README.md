Yasa
====

YASA is a high performance stat aggregation daemon which collects numeric time-series data based on given keys.
Data is then aggregated and periodically dumped to individual Round Robin Databases depending on its key.
Since YASA is able to load recently touched RRD archives into state its able to significantly reduce its I/O footprint
and quickly service requests without reading/writing to disk.
Each RRD can be queried via Yasa's WebSocket or HTTP API allowing clients to steam/update data.

Currently Yasa only supports two data types for logging data. Gauges and Counters. Gauges are used for
storing things like temperature, memory, eg. a value at a specific point in time. Counters are used for
storing rates ie. bandwidth, requests/second, etc. Below there are some API examples showing how to use
Gauges and Counters with Yasa.


QUICK START
-----------

Have a recent version of erlang installed on your system,
	
	git clone https://github.com/Spawnfest2012/Team-Yasa.git; make; start.sh

Then goto http://localhost:8080 for regular client or http://localhost:8080/ws for websocket client. You can see the vmstats of the node yu just start and and new gauges/counters by following the api.

API Examples
============

HTTP API
--------

To set a gauge:

	/api/set?key=key.name&value=<integer_value>

To increment a counter:

	/api/incr?key=key.name&value=<integer_value>

Get time series data for a given key:

	/api/get?key=key.name&range=-<integer_value><hour|min|sec|day|month|year>


WebSocket API
-------------

You can connect to Yasa via a Websocket by opening up `/wsapi`

The Websocket API allows you to register and listen for multiple keys.

	{method: "register", key: "keyname", range: "-<integer_value><hour|min|sec|day|month|year>"}

To unregister from a registered feed.

	{method: "unregister", key: "keyname"}

To increment a counter:

	{method: "incr", key: "keyname", value: <integer_value> }

To set a gauge:

	{method: "set", key: "keyname", value: <integer_value> }

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

Final Notes
===========

This project is far from finished. There's still plenty of bugs to fix and tests to write
if you a take a peek at our code. Were open to any suggestions you many have about future
work on this project. Maybe its a lost cause, maybe its not :) Please let us know!
