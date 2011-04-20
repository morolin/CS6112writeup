def plus(a):
	return (lambda b : a + b)

def fail(reason):
	raise Exception(reason)

def _default(a, maybe):
	if maybe is not None:
		maybe
	else
		a
