-record(rra_queue, {
	queue :: queue(),
	step = 0 :: integer(),
	size = 0 :: integer(),
	ratio = 1 :: integer(),
	previous_queue = undefined :: undefined | queue()
}).