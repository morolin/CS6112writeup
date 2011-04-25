def plus(a):
	return (lambda b : a + b)

def land(a):
	return (lambda b : a and b)

def fail(reason):
	raise Exception(reason)

def _default(a, maybe):
	if maybe is not None:
		maybe
	else:
		a

def _match_failure(info):
	raise Exception(info)

def _is_pair(x):
	return isinstance(x, tuple) and len(x) == 2

def _equals(a):
	return (lambda b : a == b)
