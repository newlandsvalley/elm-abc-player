var Elm = Elm || { Native: {} };
Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a / b) | 0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error('Cannot perform mod 0. Division by zero error.');
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a, b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a, b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n, lo) < 0 ? lo : Utils.cmp(n, hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity;
	}

	function truncate(n)
	{
		return n | 0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees: degrees,
		turns: turns,
		fromPolar: fromPolar,
		toPolar: toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Port = {};

Elm.Native.Port.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;

	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				'Port Error:\n' +
				'No argument was given for the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You need to provide an initial value!\n\n' +
				'Find out more about ports here <http://elm-lang.org/learn/Ports.elm>'
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				'Port Error:\n' +
				'Regarding the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You just sent the value:\n\n' +
				'    ' + JSON.stringify(value) + '\n\n' +
				'but it cannot be converted to the necessary type.\n' +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};

if (!Elm.fullscreen) {
	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];

				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild === "function")
				{
					container.appendChild(errorNode(error.message));
				}
				else
				{
					console.error(error.message);
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		}

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode('Open the developer console for more details.'));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.ctor === 'Element_elm_builtin')
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000 / 60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							'Unexpected draw callback.\n' +
							'Please report this to <https://github.com/elm-lang/core/issues>.'
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld, nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld, nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b); }; }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a, b, c); }; };
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a, b, c, d); }; }; };
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a, b, c, d, e, f); }; }; }; }; };
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun, a, b)
	{
		return fun.arity === 2
			? fun.func(a, b)
			: fun(a)(b);
	}
	function A3(fun, a, b, c)
	{
		return fun.arity === 3
			? fun.func(a, b, c)
			: fun(a)(b)(c);
	}
	function A4(fun, a, b, c, d)
	{
		return fun.arity === 4
			? fun.func(a, b, c, d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun, a, b, c, d, e)
	{
		return fun.arity === 5
			? fun.func(a, b, c, d, e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun, a, b, c, d, e, f)
	{
		return fun.arity === 6
			? fun.func(a, b, c, d, e, f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun, a, b, c, d, e, f, g)
	{
		return fun.arity === 7
			? fun.func(a, b, c, d, e, f, g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun, a, b, c, d, e, f, g, h)
	{
		return fun.arity === 8
			? fun.func(a, b, c, d, e, f, g, h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun, a, b, c, d, e, f, g, h, i)
	{
		return fun.arity === 9
			? fun.func(a, b, c, d, e, f, g, h, i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}


	// COMPARISONS

	function eq(l, r)
	{
		var stack = [{'x': l, 'y': r}];
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === 'object')
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT', 'EQ', 'GT'];

	function compare(x, y)
	{
		return {
			ctor: ord[cmp(x, y) + 1]
		};
	}

	function cmp(x, y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === '::' || x.ctor === '[]')
		{
			while (true)
			{
				if (x.ctor === '[]' && y.ctor === '[]')
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0, 6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	// TUPLES

	var Tuple0 = {
		ctor: '_Tuple0'
	};

	function Tuple2(x, y)
	{
		return {
			ctor: '_Tuple2',
			_0: x,
			_1: y
		};
	}


	// LITERALS

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}


	// GUID

	var count = 0;
	function guid(_)
	{
		return count++;
	}


	// RECORDS

	function update(oldRecord, updatedFields)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = (key in updatedFields) ? updatedFields[key] : oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}


	// MOUSE COORDINATES

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor: '[]' };

	function Cons(hd, tl)
	{
		return {
			ctor: '::',
			_0: hd,
			_1: tl
		};
	}

	function list(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo, hi)
	{
		var list = Nil;
		if (lo <= hi)
		{
			do
			{
				list = Cons(hi, list);
			}
			while (hi-- > lo);
		}
		return list;
	}

	function append(xs, ys)
	{
		// append Strings
		if (typeof xs === 'string')
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0, 5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}


		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}


	// CRASHES

	function crash(moduleName, region)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function crashCase(moduleName, region, value)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
				+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
				+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function regionToString(region)
	{
		if (region.start.line == region.end.line)
		{
			return 'on line ' + region.start.line;
		}
		return 'between lines ' + region.start.line + ' and ' + region.end.line;
	}


	// BAD PORTS

	function badPort(expected, received)
	{
		throw new Error(
			'Runtime error when sending values through a port.\n\n'
			+ 'Expecting ' + expected + ' but was given ' + formatValue(received)
		);
	}

	function formatValue(value)
	{
		// Explicity format undefined values as "undefined"
		// because JSON.stringify(undefined) unhelpfully returns ""
		return (value === undefined) ? "undefined" : JSON.stringify(value);
	}


	// TO STRING

	var _Array;
	var Dict;
	var List;

	var toString = function(v)
	{
		var type = typeof v;
		if (type === 'function')
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === 'boolean')
		{
			return v ? 'True' : 'False';
		}
		else if (type === 'number')
		{
			return v + '';
		}
		else if ((v instanceof String) && v.isChar)
		{
			return '\'' + addSlashes(v, true) + '\'';
		}
		else if (type === 'string')
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === 'object' && 'ctor' in v)
		{
			if (v.ctor.substring(0, 6) === '_Tuple')
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return '(' + output.join(',') + ')';
			}
			else if (v.ctor === '_Array')
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return 'Array.fromList ' + toString(list);
			}
			else if (v.ctor === '::')
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === '::')
				{
					output += ',' + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === '[]')
			{
				return '[]';
			}
			else if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin' || v.ctor === 'Set_elm_builtin')
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				var list;
				var name;
				if (v.ctor === 'Set_elm_builtin')
				{
					if (!List)
					{
						List = Elm.List.make(localRuntime);
					}
					name = 'Set';
					list = A2(List.map, function(x) {return x._0; }, Dict.toList(v._0));
				}
				else
				{
					name = 'Dict';
					list = Dict.toList(v);
				}
				return name + '.fromList ' + toString(list);
			}
			else if (v.ctor.slice(0, 5) === 'Text:')
			{
				return '<text>';
			}
			else if (v.ctor === 'Element_elm_builtin')
			{
				return '<element>'
			}
			else if (v.ctor === 'Form_elm_builtin')
			{
				return '<form>'
			}
			else
			{
				var output = '';
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		else if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return '<signal>';
		}
		else if (type === 'object')
		{
			var output = [];
			for (var k in v)
			{
				output.push(k + ' = ' + toString(v[k]));
			}
			if (output.length === 0)
			{
				return '{}';
			}
			return '{ ' + output.join(', ') + ' }';
		}
		return '<internal structure>';
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, '\\\'');
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		update: update,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		list: list,
		range: range,
		append: F2(append),

		crash: crash,
		crashCase: crashCase,
		badPort: badPort,

		toString: toString
	};
};

Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values) return _elm.Basics.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var _op = {};
   var uncurry = F2(function (f,_p0) {
      var _p1 = _p0;
      return A2(f,_p1._0,_p1._1);
   });
   var curry = F3(function (f,a,b) {
      return f({ctor: "_Tuple2",_0: a,_1: b});
   });
   var flip = F3(function (f,b,a) {    return A2(f,a,b);});
   var snd = function (_p2) {    var _p3 = _p2;return _p3._1;};
   var fst = function (_p4) {    var _p5 = _p4;return _p5._0;};
   var always = F2(function (a,_p6) {    return a;});
   var identity = function (x) {    return x;};
   _op["<|"] = F2(function (f,x) {    return f(x);});
   _op["|>"] = F2(function (x,f) {    return f(x);});
   _op[">>"] = F3(function (f,g,x) {    return g(f(x));});
   _op["<<"] = F3(function (g,f,x) {    return g(f(x));});
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Utils.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {    return t;};
   return _elm.Basics.values = {_op: _op
                               ,max: max
                               ,min: min
                               ,compare: compare
                               ,not: not
                               ,xor: xor
                               ,rem: rem
                               ,negate: negate
                               ,abs: abs
                               ,sqrt: sqrt
                               ,clamp: clamp
                               ,logBase: logBase
                               ,e: e
                               ,pi: pi
                               ,cos: cos
                               ,sin: sin
                               ,tan: tan
                               ,acos: acos
                               ,asin: asin
                               ,atan: atan
                               ,atan2: atan2
                               ,round: round
                               ,floor: floor
                               ,ceiling: ceiling
                               ,truncate: truncate
                               ,toFloat: toFloat
                               ,degrees: degrees
                               ,radians: radians
                               ,turns: turns
                               ,toPolar: toPolar
                               ,fromPolar: fromPolar
                               ,isNaN: isNaN
                               ,isInfinite: isInfinite
                               ,toString: toString
                               ,fst: fst
                               ,snd: snd
                               ,identity: identity
                               ,always: always
                               ,flip: flip
                               ,curry: curry
                               ,uncurry: uncurry
                               ,LT: LT
                               ,EQ: EQ
                               ,GT: GT};
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values) return _elm.Maybe.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var withDefault = F2(function ($default,maybe) {
      var _p0 = maybe;
      if (_p0.ctor === "Just") {
            return _p0._0;
         } else {
            return $default;
         }
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      oneOf: while (true) {
         var _p1 = maybes;
         if (_p1.ctor === "[]") {
               return Nothing;
            } else {
               var _p3 = _p1._0;
               var _p2 = _p3;
               if (_p2.ctor === "Nothing") {
                     var _v3 = _p1._1;
                     maybes = _v3;
                     continue oneOf;
                  } else {
                     return _p3;
                  }
            }
      }
   };
   var andThen = F2(function (maybeValue,callback) {
      var _p4 = maybeValue;
      if (_p4.ctor === "Just") {
            return callback(_p4._0);
         } else {
            return Nothing;
         }
   });
   var Just = function (a) {    return {ctor: "Just",_0: a};};
   var map = F2(function (f,maybe) {
      var _p5 = maybe;
      if (_p5.ctor === "Just") {
            return Just(f(_p5._0));
         } else {
            return Nothing;
         }
   });
   var map2 = F3(function (func,ma,mb) {
      var _p6 = {ctor: "_Tuple2",_0: ma,_1: mb};
      if (_p6.ctor === "_Tuple2" && _p6._0.ctor === "Just" && _p6._1.ctor === "Just")
      {
            return Just(A2(func,_p6._0._0,_p6._1._0));
         } else {
            return Nothing;
         }
   });
   var map3 = F4(function (func,ma,mb,mc) {
      var _p7 = {ctor: "_Tuple3",_0: ma,_1: mb,_2: mc};
      if (_p7.ctor === "_Tuple3" && _p7._0.ctor === "Just" && _p7._1.ctor === "Just" && _p7._2.ctor === "Just")
      {
            return Just(A3(func,_p7._0._0,_p7._1._0,_p7._2._0));
         } else {
            return Nothing;
         }
   });
   var map4 = F5(function (func,ma,mb,mc,md) {
      var _p8 = {ctor: "_Tuple4",_0: ma,_1: mb,_2: mc,_3: md};
      if (_p8.ctor === "_Tuple4" && _p8._0.ctor === "Just" && _p8._1.ctor === "Just" && _p8._2.ctor === "Just" && _p8._3.ctor === "Just")
      {
            return Just(A4(func,
            _p8._0._0,
            _p8._1._0,
            _p8._2._0,
            _p8._3._0));
         } else {
            return Nothing;
         }
   });
   var map5 = F6(function (func,ma,mb,mc,md,me) {
      var _p9 = {ctor: "_Tuple5"
                ,_0: ma
                ,_1: mb
                ,_2: mc
                ,_3: md
                ,_4: me};
      if (_p9.ctor === "_Tuple5" && _p9._0.ctor === "Just" && _p9._1.ctor === "Just" && _p9._2.ctor === "Just" && _p9._3.ctor === "Just" && _p9._4.ctor === "Just")
      {
            return Just(A5(func,
            _p9._0._0,
            _p9._1._0,
            _p9._2._0,
            _p9._3._0,
            _p9._4._0));
         } else {
            return Nothing;
         }
   });
   return _elm.Maybe.values = {_op: _op
                              ,andThen: andThen
                              ,map: map
                              ,map2: map2
                              ,map3: map3
                              ,map4: map4
                              ,map5: map5
                              ,withDefault: withDefault
                              ,oneOf: oneOf
                              ,Just: Just
                              ,Nothing: Nothing};
};
Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	var fromArray = Utils.list;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil: Nil,
		Cons: Cons,
		cons: F2(Cons),
		toArray: toArray,
		fromArray: fromArray,

		foldl: F3(foldl),
		foldr: F3(foldr),

		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		sortBy: F2(sortBy),
		sortWith: F2(sortWith),
		take: F2(take)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;
};

Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values) return _elm.List.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var _op = {};
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {
      return A2(sortBy,$Basics.identity,xs);
   };
   var drop = F2(function (n,list) {
      drop: while (true) if (_U.cmp(n,0) < 1) return list; else {
            var _p0 = list;
            if (_p0.ctor === "[]") {
                  return list;
               } else {
                  var _v1 = n - 1,_v2 = _p0._1;
                  n = _v1;
                  list = _v2;
                  continue drop;
               }
         }
   });
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = F2(function (isOkay,list) {
      any: while (true) {
         var _p1 = list;
         if (_p1.ctor === "[]") {
               return false;
            } else {
               if (isOkay(_p1._0)) return true; else {
                     var _v4 = isOkay,_v5 = _p1._1;
                     isOkay = _v4;
                     list = _v5;
                     continue any;
                  }
            }
      }
   });
   var all = F2(function (isOkay,list) {
      return $Basics.not(A2(any,
      function (_p2) {
         return $Basics.not(isOkay(_p2));
      },
      list));
   });
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {
      return A3(foldl,
      F2(function (_p3,i) {    return i + 1;}),
      0,
      xs);
   };
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {    return x + y;}),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {    return x * y;}),
      1,
      numbers);
   };
   var maximum = function (list) {
      var _p4 = list;
      if (_p4.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.max,_p4._0,_p4._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var minimum = function (list) {
      var _p5 = list;
      if (_p5.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.min,_p5._0,_p5._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var indexedMap = F2(function (f,xs) {
      return A3(map2,f,_U.range(0,length(xs) - 1),xs);
   });
   var member = F2(function (x,xs) {
      return A2(any,function (a) {    return _U.eq(a,x);},xs);
   });
   var isEmpty = function (xs) {
      var _p6 = xs;
      if (_p6.ctor === "[]") {
            return true;
         } else {
            return false;
         }
   };
   var tail = function (list) {
      var _p7 = list;
      if (_p7.ctor === "::") {
            return $Maybe.Just(_p7._1);
         } else {
            return $Maybe.Nothing;
         }
   };
   var head = function (list) {
      var _p8 = list;
      if (_p8.ctor === "::") {
            return $Maybe.Just(_p8._0);
         } else {
            return $Maybe.Nothing;
         }
   };
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {
      return A3(foldr,
      F2(function (x,acc) {    return A2(_op["::"],f(x),acc);}),
      _U.list([]),
      xs);
   });
   var filter = F2(function (pred,xs) {
      var conditionalCons = F2(function (x,xs$) {
         return pred(x) ? A2(_op["::"],x,xs$) : xs$;
      });
      return A3(foldr,conditionalCons,_U.list([]),xs);
   });
   var maybeCons = F3(function (f,mx,xs) {
      var _p9 = f(mx);
      if (_p9.ctor === "Just") {
            return A2(_op["::"],_p9._0,xs);
         } else {
            return xs;
         }
   });
   var filterMap = F2(function (f,xs) {
      return A3(foldr,maybeCons(f),_U.list([]),xs);
   });
   var reverse = function (list) {
      return A3(foldl,
      F2(function (x,y) {    return A2(_op["::"],x,y);}),
      _U.list([]),
      list);
   };
   var scanl = F3(function (f,b,xs) {
      var scan1 = F2(function (x,accAcc) {
         var _p10 = accAcc;
         if (_p10.ctor === "::") {
               return A2(_op["::"],A2(f,x,_p10._0),accAcc);
            } else {
               return _U.list([]);
            }
      });
      return reverse(A3(foldl,scan1,_U.list([b]),xs));
   });
   var append = F2(function (xs,ys) {
      var _p11 = ys;
      if (_p11.ctor === "[]") {
            return xs;
         } else {
            return A3(foldr,
            F2(function (x,y) {    return A2(_op["::"],x,y);}),
            ys,
            xs);
         }
   });
   var concat = function (lists) {
      return A3(foldr,append,_U.list([]),lists);
   };
   var concatMap = F2(function (f,list) {
      return concat(A2(map,f,list));
   });
   var partition = F2(function (pred,list) {
      var step = F2(function (x,_p12) {
         var _p13 = _p12;
         var _p15 = _p13._0;
         var _p14 = _p13._1;
         return pred(x) ? {ctor: "_Tuple2"
                          ,_0: A2(_op["::"],x,_p15)
                          ,_1: _p14} : {ctor: "_Tuple2"
                                       ,_0: _p15
                                       ,_1: A2(_op["::"],x,_p14)};
      });
      return A3(foldr,
      step,
      {ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},
      list);
   });
   var unzip = function (pairs) {
      var step = F2(function (_p17,_p16) {
         var _p18 = _p17;
         var _p19 = _p16;
         return {ctor: "_Tuple2"
                ,_0: A2(_op["::"],_p18._0,_p19._0)
                ,_1: A2(_op["::"],_p18._1,_p19._1)};
      });
      return A3(foldr,
      step,
      {ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},
      pairs);
   };
   var intersperse = F2(function (sep,xs) {
      var _p20 = xs;
      if (_p20.ctor === "[]") {
            return _U.list([]);
         } else {
            var step = F2(function (x,rest) {
               return A2(_op["::"],sep,A2(_op["::"],x,rest));
            });
            var spersed = A3(foldr,step,_U.list([]),_p20._1);
            return A2(_op["::"],_p20._0,spersed);
         }
   });
   var repeatHelp = F3(function (result,n,value) {
      repeatHelp: while (true) if (_U.cmp(n,0) < 1) return result;
      else {
            var _v18 = A2(_op["::"],value,result),
            _v19 = n - 1,
            _v20 = value;
            result = _v18;
            n = _v19;
            value = _v20;
            continue repeatHelp;
         }
   });
   var repeat = F2(function (n,value) {
      return A3(repeatHelp,_U.list([]),n,value);
   });
   return _elm.List.values = {_op: _op
                             ,isEmpty: isEmpty
                             ,length: length
                             ,reverse: reverse
                             ,member: member
                             ,head: head
                             ,tail: tail
                             ,filter: filter
                             ,take: take
                             ,drop: drop
                             ,repeat: repeat
                             ,append: append
                             ,concat: concat
                             ,intersperse: intersperse
                             ,partition: partition
                             ,unzip: unzip
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,filterMap: filterMap
                             ,concatMap: concatMap
                             ,indexedMap: indexedMap
                             ,foldr: foldr
                             ,foldl: foldl
                             ,sum: sum
                             ,product: product
                             ,maximum: maximum
                             ,minimum: minimum
                             ,all: all
                             ,any: any
                             ,scanl: scanl
                             ,sort: sort
                             ,sortBy: sortBy
                             ,sortWith: sortWith};
};
Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1, 0, 0, 0, 1, 0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t, m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11 * c + m12 * s, -m11 * s + m12 * c, m[2],
					  m21 * c + m22 * s, -m21 * s + m22 * c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11 * n11 + m12 * n21,
					  m11 * n12 + m12 * n22,
					  m11 * ndx + m12 * ndy + mdx,
					  m21 * n11 + m22 * n21,
					  m21 * n12 + m22 * n22,
					  m21 * ndx + m22 * ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity: identity,
		matrix: F6(matrix),
		rotation: rotation,
		multiply: F2(multiply)
		/*
		transform: F7(transform),
		rotate: F2(rotate),
		move: F2(move),
		scale: F2(scale),
		scaleX: F2(scaleX),
		scaleY: F2(scaleY),
		reflectX: reflectX,
		reflectY: reflectY
		*/
	};
};

Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values) return _elm.Transform2D.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var _op = {};
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,y) {
      return A6(matrix,1,0,0,1,x,y);
   });
   var scale = function (s) {    return A6(matrix,s,0,0,s,0,0);};
   var scaleX = function (x) {    return A6(matrix,x,0,0,1,0,0);};
   var scaleY = function (y) {    return A6(matrix,1,0,0,y,0,0);};
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   return _elm.Transform2D.values = {_op: _op
                                    ,identity: identity
                                    ,matrix: matrix
                                    ,multiply: multiply
                                    ,rotation: rotation
                                    ,translation: translation
                                    ,scale: scale
                                    ,scaleX: scaleX
                                    ,scaleY: scaleY};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(redo, ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx, style, path)
	{
		if (style.dashing.ctor === '[]')
		{
			trace(ctx, path);
		}
		else
		{
			customLineHelp(ctx, style, path);
		}
		ctx.scale(1, -1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1 = 0, y1 = 0, dx = 0, dy = 0, remaining = 0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0, y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(redo, ctx, style);
		ctx.scale(1, -1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0, -2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w / 2,
			destY = -h / 2,
			destW = w,
			destH = h;

		ctx.scale(1, -1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta % (Math.PI * 2));
		}
		if (scale !== 1)
		{
			ctx.scale(scale, scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta;
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i]._0.form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i]._0;
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w, h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = 'block';
		canvas.style.position = 'absolute';
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w, h, div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w, h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem._0.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w, h, div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w, h, forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w: w, h: h, forms: forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};
};

Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values) return _elm.Color.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm);
   var _op = {};
   var Radial = F5(function (a,b,c,d,e) {
      return {ctor: "Radial",_0: a,_1: b,_2: c,_3: d,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,b,c) {
      return {ctor: "Linear",_0: a,_1: b,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      var integer = $Basics.floor(f);
      return $Basics.toFloat(A2($Basics._op["%"],
      integer,
      n)) + f - $Basics.toFloat(integer);
   });
   var rgbToHsl = F3(function (red,green,blue) {
      var b = $Basics.toFloat(blue) / 255;
      var g = $Basics.toFloat(green) / 255;
      var r = $Basics.toFloat(red) / 255;
      var cMax = A2($Basics.max,A2($Basics.max,r,g),b);
      var cMin = A2($Basics.min,A2($Basics.min,r,g),b);
      var c = cMax - cMin;
      var lightness = (cMax + cMin) / 2;
      var saturation = _U.eq(lightness,
      0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
      var hue = $Basics.degrees(60) * (_U.eq(cMax,r) ? A2(fmod,
      (g - b) / c,
      6) : _U.eq(cMax,g) ? (b - r) / c + 2 : (r - g) / c + 4);
      return {ctor: "_Tuple3",_0: hue,_1: saturation,_2: lightness};
   });
   var hslToRgb = F3(function (hue,saturation,lightness) {
      var hue$ = hue / $Basics.degrees(60);
      var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
      var x = chroma * (1 - $Basics.abs(A2(fmod,hue$,2) - 1));
      var _p0 = _U.cmp(hue$,0) < 0 ? {ctor: "_Tuple3"
                                     ,_0: 0
                                     ,_1: 0
                                     ,_2: 0} : _U.cmp(hue$,1) < 0 ? {ctor: "_Tuple3"
                                                                    ,_0: chroma
                                                                    ,_1: x
                                                                    ,_2: 0} : _U.cmp(hue$,2) < 0 ? {ctor: "_Tuple3"
                                                                                                   ,_0: x
                                                                                                   ,_1: chroma
                                                                                                   ,_2: 0} : _U.cmp(hue$,3) < 0 ? {ctor: "_Tuple3"
                                                                                                                                  ,_0: 0
                                                                                                                                  ,_1: chroma
                                                                                                                                  ,_2: x} : _U.cmp(hue$,
      4) < 0 ? {ctor: "_Tuple3",_0: 0,_1: x,_2: chroma} : _U.cmp(hue$,
      5) < 0 ? {ctor: "_Tuple3",_0: x,_1: 0,_2: chroma} : _U.cmp(hue$,
      6) < 0 ? {ctor: "_Tuple3"
               ,_0: chroma
               ,_1: 0
               ,_2: x} : {ctor: "_Tuple3",_0: 0,_1: 0,_2: 0};
      var r = _p0._0;
      var g = _p0._1;
      var b = _p0._2;
      var m = lightness - chroma / 2;
      return {ctor: "_Tuple3",_0: r + m,_1: g + m,_2: b + m};
   });
   var toRgb = function (color) {
      var _p1 = color;
      if (_p1.ctor === "RGBA") {
            return {red: _p1._0
                   ,green: _p1._1
                   ,blue: _p1._2
                   ,alpha: _p1._3};
         } else {
            var _p2 = A3(hslToRgb,_p1._0,_p1._1,_p1._2);
            var r = _p2._0;
            var g = _p2._1;
            var b = _p2._2;
            return {red: $Basics.round(255 * r)
                   ,green: $Basics.round(255 * g)
                   ,blue: $Basics.round(255 * b)
                   ,alpha: _p1._3};
         }
   };
   var toHsl = function (color) {
      var _p3 = color;
      if (_p3.ctor === "HSLA") {
            return {hue: _p3._0
                   ,saturation: _p3._1
                   ,lightness: _p3._2
                   ,alpha: _p3._3};
         } else {
            var _p4 = A3(rgbToHsl,_p3._0,_p3._1,_p3._2);
            var h = _p4._0;
            var s = _p4._1;
            var l = _p4._2;
            return {hue: h,saturation: s,lightness: l,alpha: _p3._3};
         }
   };
   var HSLA = F4(function (a,b,c,d) {
      return {ctor: "HSLA",_0: a,_1: b,_2: c,_3: d};
   });
   var hsla = F4(function (hue,saturation,lightness,alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,saturation,lightness) {
      return A4(hsla,hue,saturation,lightness,1);
   });
   var complement = function (color) {
      var _p5 = color;
      if (_p5.ctor === "HSLA") {
            return A4(hsla,
            _p5._0 + $Basics.degrees(180),
            _p5._1,
            _p5._2,
            _p5._3);
         } else {
            var _p6 = A3(rgbToHsl,_p5._0,_p5._1,_p5._2);
            var h = _p6._0;
            var s = _p6._1;
            var l = _p6._2;
            return A4(hsla,h + $Basics.degrees(180),s,l,_p5._3);
         }
   };
   var grayscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var greyscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var RGBA = F4(function (a,b,c,d) {
      return {ctor: "RGBA",_0: a,_1: b,_2: c,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {    return A4(RGBA,r,g,b,1);});
   var lightRed = A4(RGBA,239,41,41,1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,164,0,0,1);
   var lightOrange = A4(RGBA,252,175,62,1);
   var orange = A4(RGBA,245,121,0,1);
   var darkOrange = A4(RGBA,206,92,0,1);
   var lightYellow = A4(RGBA,255,233,79,1);
   var yellow = A4(RGBA,237,212,0,1);
   var darkYellow = A4(RGBA,196,160,0,1);
   var lightGreen = A4(RGBA,138,226,52,1);
   var green = A4(RGBA,115,210,22,1);
   var darkGreen = A4(RGBA,78,154,6,1);
   var lightBlue = A4(RGBA,114,159,207,1);
   var blue = A4(RGBA,52,101,164,1);
   var darkBlue = A4(RGBA,32,74,135,1);
   var lightPurple = A4(RGBA,173,127,168,1);
   var purple = A4(RGBA,117,80,123,1);
   var darkPurple = A4(RGBA,92,53,102,1);
   var lightBrown = A4(RGBA,233,185,110,1);
   var brown = A4(RGBA,193,125,17,1);
   var darkBrown = A4(RGBA,143,89,2,1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,255,255,255,1);
   var lightGrey = A4(RGBA,238,238,236,1);
   var grey = A4(RGBA,211,215,207,1);
   var darkGrey = A4(RGBA,186,189,182,1);
   var lightGray = A4(RGBA,238,238,236,1);
   var gray = A4(RGBA,211,215,207,1);
   var darkGray = A4(RGBA,186,189,182,1);
   var lightCharcoal = A4(RGBA,136,138,133,1);
   var charcoal = A4(RGBA,85,87,83,1);
   var darkCharcoal = A4(RGBA,46,52,54,1);
   return _elm.Color.values = {_op: _op
                              ,rgb: rgb
                              ,rgba: rgba
                              ,hsl: hsl
                              ,hsla: hsla
                              ,greyscale: greyscale
                              ,grayscale: grayscale
                              ,complement: complement
                              ,linear: linear
                              ,radial: radial
                              ,toRgb: toRgb
                              ,toHsl: toHsl
                              ,red: red
                              ,orange: orange
                              ,yellow: yellow
                              ,green: green
                              ,blue: blue
                              ,purple: purple
                              ,brown: brown
                              ,lightRed: lightRed
                              ,lightOrange: lightOrange
                              ,lightYellow: lightYellow
                              ,lightGreen: lightGreen
                              ,lightBlue: lightBlue
                              ,lightPurple: lightPurple
                              ,lightBrown: lightBrown
                              ,darkRed: darkRed
                              ,darkOrange: darkOrange
                              ,darkYellow: darkYellow
                              ,darkGreen: darkGreen
                              ,darkBlue: darkBlue
                              ,darkPurple: darkPurple
                              ,darkBrown: darkBrown
                              ,white: white
                              ,lightGrey: lightGrey
                              ,grey: grey
                              ,darkGrey: darkGrey
                              ,lightCharcoal: lightCharcoal
                              ,charcoal: charcoal
                              ,darkCharcoal: darkCharcoal
                              ,black: black
                              ,lightGray: lightGray
                              ,gray: gray
                              ,darkGray: darkGray};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	var createNode =
		typeof document === 'undefined'
			?
				function(_)
				{
					return {
						style: {},
						appendChild: function() {}
					};
				}
			:
				function(elementType)
				{
					var node = document.createElement(elementType);
					node.style.padding = '0';
					node.style.margin = '0';
					return node;
				}
			;


	function newElement(width, height, elementPrim)
	{
		return {
			ctor: 'Element_elm_builtin',
			_0: {
				element: elementPrim,
				props: {
					id: Utils.guid(),
					width: width,
					height: height,
					opacity: 1,
					color: Maybe.Nothing,
					href: '',
					tag: '',
					hover: Utils.Tuple0,
					click: Utils.Tuple0
				}
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width | 0) + 'px';
		node.style.height = (height | 0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img, props.width, props.height, img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = 'block';
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = 'hidden';

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw) | 0) + 'px';
			img.style.height = ((this.height * sh) | 0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw) | 0) + 'px';
			img.style.marginTop = ((- pos._1 * sh) | 0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp: goDown,
		DDown: goDown,
		DLeft: goRight,
		DRight: goRight,
		DIn: goOut,
		DOut: goOut
	};
	function needsReversal(dir)
	{
		return dir === 'DUp' || dir === 'DLeft' || dir === 'DIn';
	}

	function flow(dir, elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir === goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === 'Absolute'
			? pos._0 + 'px'
			: (pos._0 * 100) + '%';
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos, wrappedElement, e)
	{
		var elem = wrappedElement._0;
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w / 2) | 0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h / 2) | 0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos, elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = 'hidden';
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(wrappedElement)
	{
		var elem = wrappedElement._0;
		return setProps(elem, makeElement(elem));
	}

	function makeElement(e)
	{
		var elem = e.element;
		switch (elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, wrappedCurrent, wrappedNext)
	{
		var curr = wrappedCurrent._0;
		var next = wrappedNext._0;
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(wrappedNext);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch (nextE.ctor)
		{
			case 'Spacer':
				updateProps(node, curr, next);
				return rootNode;

			case 'RawHtml':
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Image':
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE, currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(wrappedNext);
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Flow':
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i]._0.element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(wrappedNext);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(wrappedNext);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i--; )
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Container':
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case 'Custom':
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(wrappedNext);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width | 0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height | 0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor: 'RawHtml',
				html: Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		};
	}

	function markdown(text)
	{
		var raw = {
			ctor: 'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	var htmlHeight =
		typeof document !== 'undefined'
			? realHtmlHeight
			: function(a, b) { return Utils.Tuple2(0, 0); };

	function realHtmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + 'px';
		}
		temp.style.visibility = 'hidden';
		temp.style.styleFloat = 'left';
		temp.style.cssFloat = 'left';

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue('width').slice(0, -2) - 0);
		var h = Math.ceil(style.getPropertyValue('height').slice(0, -2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w, h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== 'Nothing')
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas.href;
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length === 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  '&#38;')
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  '&#39;')
			.replace(/</g,  '&#60;')
			.replace(/>/g,  '&#62;');
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length === 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] === ' ')
		{
			arr[0] = '&nbsp;';
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] === ' ' && arr[i - 1] === ' ')
			{
				arr[i - 1] = arr[i - 1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] === ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length - 1] === ' ')
		{
			return arr.slice(0, -1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values) return _elm.Text.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var _op = {};
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,append,empty,texts);
   };
   var join = F2(function (seperator,texts) {
      return concat(A2($List.intersperse,seperator,texts));
   });
   var defaultStyle = {typeface: _U.list([])
                      ,height: $Maybe.Nothing
                      ,color: $Color.black
                      ,bold: false
                      ,italic: false
                      ,line: $Maybe.Nothing};
   var Style = F6(function (a,b,c,d,e,f) {
      return {typeface: a
             ,height: b
             ,color: c
             ,bold: d
             ,italic: e
             ,line: f};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   return _elm.Text.values = {_op: _op
                             ,fromString: fromString
                             ,empty: empty
                             ,append: append
                             ,concat: concat
                             ,join: join
                             ,link: link
                             ,style: style
                             ,defaultStyle: defaultStyle
                             ,typeface: typeface
                             ,monospace: monospace
                             ,height: height
                             ,color: color
                             ,bold: bold
                             ,italic: italic
                             ,line: line
                             ,Style: Style
                             ,Under: Under
                             ,Over: Over
                             ,Through: Through};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var _op = {};
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var RawPosition = F4(function (a,b,c,d) {
      return {horizontal: a,vertical: b,x: c,y: d};
   });
   var Position = function (a) {
      return {ctor: "Position",_0: a};
   };
   var Relative = function (a) {
      return {ctor: "Relative",_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute",_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeft = Position({horizontal: N
                             ,vertical: N
                             ,x: Absolute(0)
                             ,y: Absolute(0)});
   var bottomLeftAt = F2(function (x,y) {
      return Position({horizontal: N,vertical: N,x: x,y: y});
   });
   var Z = {ctor: "Z"};
   var middle = Position({horizontal: Z
                         ,vertical: Z
                         ,x: Relative(0.5)
                         ,y: Relative(0.5)});
   var midLeft = Position({horizontal: N
                          ,vertical: Z
                          ,x: Absolute(0)
                          ,y: Relative(0.5)});
   var midBottom = Position({horizontal: Z
                            ,vertical: N
                            ,x: Relative(0.5)
                            ,y: Absolute(0)});
   var middleAt = F2(function (x,y) {
      return Position({horizontal: Z,vertical: Z,x: x,y: y});
   });
   var midLeftAt = F2(function (x,y) {
      return Position({horizontal: N,vertical: Z,x: x,y: y});
   });
   var midBottomAt = F2(function (x,y) {
      return Position({horizontal: Z,vertical: N,x: x,y: y});
   });
   var P = {ctor: "P"};
   var topLeft = Position({horizontal: N
                          ,vertical: P
                          ,x: Absolute(0)
                          ,y: Absolute(0)});
   var topRight = Position({horizontal: P
                           ,vertical: P
                           ,x: Absolute(0)
                           ,y: Absolute(0)});
   var bottomRight = Position({horizontal: P
                              ,vertical: N
                              ,x: Absolute(0)
                              ,y: Absolute(0)});
   var midRight = Position({horizontal: P
                           ,vertical: Z
                           ,x: Absolute(0)
                           ,y: Relative(0.5)});
   var midTop = Position({horizontal: Z
                         ,vertical: P
                         ,x: Relative(0.5)
                         ,y: Absolute(0)});
   var topLeftAt = F2(function (x,y) {
      return Position({horizontal: N,vertical: P,x: x,y: y});
   });
   var topRightAt = F2(function (x,y) {
      return Position({horizontal: P,vertical: P,x: x,y: y});
   });
   var bottomRightAt = F2(function (x,y) {
      return Position({horizontal: P,vertical: N,x: x,y: y});
   });
   var midRightAt = F2(function (x,y) {
      return Position({horizontal: P,vertical: Z,x: x,y: y});
   });
   var midTopAt = F2(function (x,y) {
      return Position({horizontal: Z,vertical: P,x: x,y: y});
   });
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {
      return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));
   };
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped",_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow",_0: a,_1: b};
   });
   var Container = F2(function (a,b) {
      return {ctor: "Container",_0: a,_1: b};
   });
   var Image = F4(function (a,b,c,d) {
      return {ctor: "Image",_0: a,_1: b,_2: c,_3: d};
   });
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,h,src) {
      return A3(newElement,w,h,A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,h,src) {
      return A3(newElement,w,h,A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,w,h,src) {
      return A3(newElement,w,h,A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,h,src) {
      return A3(newElement,w,h,A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,h,_p0,e) {
      var _p1 = _p0;
      return A3(newElement,w,h,A2(Container,_p1._0,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,w,h,Spacer);
   });
   var sizeOf = function (_p2) {
      var _p3 = _p2;
      var _p4 = _p3._0;
      return {ctor: "_Tuple2"
             ,_0: _p4.props.width
             ,_1: _p4.props.height};
   };
   var heightOf = function (_p5) {
      var _p6 = _p5;
      return _p6._0.props.height;
   };
   var widthOf = function (_p7) {
      var _p8 = _p7;
      return _p8._0.props.width;
   };
   var above = F2(function (hi,lo) {
      return A3(newElement,
      A2($Basics.max,widthOf(hi),widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,DDown,_U.list([hi,lo])));
   });
   var below = F2(function (lo,hi) {
      return A3(newElement,
      A2($Basics.max,widthOf(hi),widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,DDown,_U.list([hi,lo])));
   });
   var beside = F2(function (lft,rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,heightOf(lft),heightOf(rht)),
      A2(Flow,right,_U.list([lft,rht])));
   });
   var layers = function (es) {
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      return A3(newElement,
      A2($Maybe.withDefault,0,$List.maximum(ws)),
      A2($Maybe.withDefault,0,$List.maximum(hs)),
      A2(Flow,DOut,es));
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,es) {
      var newFlow = F2(function (w,h) {
         return A3(newElement,w,h,A2(Flow,dir,es));
      });
      var maxOrZero = function (list) {
         return A2($Maybe.withDefault,0,$List.maximum(list));
      };
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      if (_U.eq(es,_U.list([]))) return empty; else {
            var _p9 = dir;
            switch (_p9.ctor)
            {case "DUp": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DDown": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DLeft": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DRight": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DIn": return A2(newFlow,maxOrZero(ws),maxOrZero(hs));
               default: return A2(newFlow,maxOrZero(ws),maxOrZero(hs));}
         }
   });
   var Properties = F9(function (a,b,c,d,e,f,g,h,i) {
      return {id: a
             ,width: b
             ,height: c
             ,opacity: d
             ,color: e
             ,href: f
             ,tag: g
             ,hover: h
             ,click: i};
   });
   var Element_elm_builtin = function (a) {
      return {ctor: "Element_elm_builtin",_0: a};
   };
   var width = F2(function (newWidth,_p10) {
      var _p11 = _p10;
      var _p14 = _p11._0.props;
      var _p13 = _p11._0.element;
      var newHeight = function () {
         var _p12 = _p13;
         switch (_p12.ctor)
         {case "Image":
            return $Basics.round($Basics.toFloat(_p12._2) / $Basics.toFloat(_p12._1) * $Basics.toFloat(newWidth));
            case "RawHtml":
            return $Basics.snd(A2($Native$Graphics$Element.htmlHeight,
              newWidth,
              _p13));
            default: return _p14.height;}
      }();
      return Element_elm_builtin({element: _p13
                                 ,props: _U.update(_p14,{width: newWidth,height: newHeight})});
   });
   var height = F2(function (newHeight,_p15) {
      var _p16 = _p15;
      return Element_elm_builtin({element: _p16._0.element
                                 ,props: _U.update(_p16._0.props,{height: newHeight})});
   });
   var size = F3(function (w,h,e) {
      return A2(height,h,A2(width,w,e));
   });
   var opacity = F2(function (givenOpacity,_p17) {
      var _p18 = _p17;
      return Element_elm_builtin({element: _p18._0.element
                                 ,props: _U.update(_p18._0.props,{opacity: givenOpacity})});
   });
   var color = F2(function (clr,_p19) {
      var _p20 = _p19;
      return Element_elm_builtin({element: _p20._0.element
                                 ,props: _U.update(_p20._0.props,{color: $Maybe.Just(clr)})});
   });
   var tag = F2(function (name,_p21) {
      var _p22 = _p21;
      return Element_elm_builtin({element: _p22._0.element
                                 ,props: _U.update(_p22._0.props,{tag: name})});
   });
   var link = F2(function (href,_p23) {
      var _p24 = _p23;
      return Element_elm_builtin({element: _p24._0.element
                                 ,props: _U.update(_p24._0.props,{href: href})});
   });
   return _elm.Graphics.Element.values = {_op: _op
                                         ,image: image
                                         ,fittedImage: fittedImage
                                         ,croppedImage: croppedImage
                                         ,tiledImage: tiledImage
                                         ,leftAligned: leftAligned
                                         ,rightAligned: rightAligned
                                         ,centered: centered
                                         ,justified: justified
                                         ,show: show
                                         ,width: width
                                         ,height: height
                                         ,size: size
                                         ,color: color
                                         ,opacity: opacity
                                         ,link: link
                                         ,tag: tag
                                         ,widthOf: widthOf
                                         ,heightOf: heightOf
                                         ,sizeOf: sizeOf
                                         ,flow: flow
                                         ,up: up
                                         ,down: down
                                         ,left: left
                                         ,right: right
                                         ,inward: inward
                                         ,outward: outward
                                         ,layers: layers
                                         ,above: above
                                         ,below: below
                                         ,beside: beside
                                         ,empty: empty
                                         ,spacer: spacer
                                         ,container: container
                                         ,middle: middle
                                         ,midTop: midTop
                                         ,midBottom: midBottom
                                         ,midLeft: midLeft
                                         ,midRight: midRight
                                         ,topLeft: topLeft
                                         ,topRight: topRight
                                         ,bottomLeft: bottomLeft
                                         ,bottomRight: bottomRight
                                         ,absolute: absolute
                                         ,relative: relative
                                         ,middleAt: middleAt
                                         ,midTopAt: midTopAt
                                         ,midBottomAt: midBottomAt
                                         ,midLeftAt: midLeftAt
                                         ,midRightAt: midRightAt
                                         ,topLeftAt: topLeftAt
                                         ,topRightAt: topRightAt
                                         ,bottomLeftAt: bottomLeftAt
                                         ,bottomRightAt: bottomRightAt};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var _op = {};
   var Shape = function (a) {    return {ctor: "Shape",_0: a};};
   var polygon = function (points) {    return Shape(points);};
   var rect = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      return Shape(_U.list([{ctor: "_Tuple2",_0: 0 - hw,_1: 0 - hh}
                           ,{ctor: "_Tuple2",_0: 0 - hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: 0 - hh}]));
   });
   var square = function (n) {    return A2(rect,n,n);};
   var oval = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      var n = 50;
      var t = 2 * $Basics.pi / n;
      var f = function (i) {
         return {ctor: "_Tuple2"
                ,_0: hw * $Basics.cos(t * i)
                ,_1: hh * $Basics.sin(t * i)};
      };
      return Shape(A2($List.map,f,_U.range(0,n - 1)));
   });
   var circle = function (r) {    return A2(oval,2 * r,2 * r);};
   var ngon = F2(function (n,r) {
      var m = $Basics.toFloat(n);
      var t = 2 * $Basics.pi / m;
      var f = function (i) {
         return {ctor: "_Tuple2"
                ,_0: r * $Basics.cos(t * i)
                ,_1: r * $Basics.sin(t * i)};
      };
      return Shape(A2($List.map,f,_U.range(0,m - 1)));
   });
   var Path = function (a) {    return {ctor: "Path",_0: a};};
   var path = function (ps) {    return Path(ps);};
   var segment = F2(function (p1,p2) {
      return Path(_U.list([p1,p2]));
   });
   var collage = $Native$Graphics$Collage.collage;
   var Fill = function (a) {    return {ctor: "Fill",_0: a};};
   var Line = function (a) {    return {ctor: "Line",_0: a};};
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup",_0: a,_1: b};
   });
   var FElement = function (a) {
      return {ctor: "FElement",_0: a};
   };
   var FImage = F4(function (a,b,c,d) {
      return {ctor: "FImage",_0: a,_1: b,_2: c,_3: d};
   });
   var FText = function (a) {    return {ctor: "FText",_0: a};};
   var FOutlinedText = F2(function (a,b) {
      return {ctor: "FOutlinedText",_0: a,_1: b};
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape",_0: a,_1: b};
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath",_0: a,_1: b};
   });
   var LineStyle = F6(function (a,b,c,d,e,f) {
      return {color: a
             ,width: b
             ,cap: c
             ,join: d
             ,dashing: e
             ,dashOffset: f};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {    return {ctor: "Sharp",_0: a};};
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {color: $Color.black
                     ,width: 1
                     ,cap: Flat
                     ,join: Sharp(10)
                     ,dashing: _U.list([])
                     ,dashOffset: 0};
   var solid = function (clr) {
      return _U.update(defaultLine,{color: clr});
   };
   var dashed = function (clr) {
      return _U.update(defaultLine,
      {color: clr,dashing: _U.list([8,4])});
   };
   var dotted = function (clr) {
      return _U.update(defaultLine,
      {color: clr,dashing: _U.list([3,3])});
   };
   var Grad = function (a) {    return {ctor: "Grad",_0: a};};
   var Texture = function (a) {
      return {ctor: "Texture",_0: a};
   };
   var Solid = function (a) {    return {ctor: "Solid",_0: a};};
   var Form_elm_builtin = function (a) {
      return {ctor: "Form_elm_builtin",_0: a};
   };
   var form = function (f) {
      return Form_elm_builtin({theta: 0
                              ,scale: 1
                              ,x: 0
                              ,y: 0
                              ,alpha: 1
                              ,form: f});
   };
   var fill = F2(function (style,_p0) {
      var _p1 = _p0;
      return form(A2(FShape,Fill(style),_p1._0));
   });
   var filled = F2(function (color,shape) {
      return A2(fill,Solid(color),shape);
   });
   var textured = F2(function (src,shape) {
      return A2(fill,Texture(src),shape);
   });
   var gradient = F2(function (grad,shape) {
      return A2(fill,Grad(grad),shape);
   });
   var outlined = F2(function (style,_p2) {
      var _p3 = _p2;
      return form(A2(FShape,Line(style),_p3._0));
   });
   var traced = F2(function (style,_p4) {
      var _p5 = _p4;
      return form(A2(FPath,style,_p5._0));
   });
   var sprite = F4(function (w,h,pos,src) {
      return form(A4(FImage,w,h,pos,src));
   });
   var toForm = function (e) {    return form(FElement(e));};
   var group = function (fs) {
      return form(A2(FGroup,$Transform2D.identity,fs));
   };
   var groupTransform = F2(function (matrix,fs) {
      return form(A2(FGroup,matrix,fs));
   });
   var text = function (t) {    return form(FText(t));};
   var outlinedText = F2(function (ls,t) {
      return form(A2(FOutlinedText,ls,t));
   });
   var move = F2(function (_p7,_p6) {
      var _p8 = _p7;
      var _p9 = _p6;
      var _p10 = _p9._0;
      return Form_elm_builtin(_U.update(_p10,
      {x: _p10.x + _p8._0,y: _p10.y + _p8._1}));
   });
   var moveX = F2(function (x,_p11) {
      var _p12 = _p11;
      var _p13 = _p12._0;
      return Form_elm_builtin(_U.update(_p13,{x: _p13.x + x}));
   });
   var moveY = F2(function (y,_p14) {
      var _p15 = _p14;
      var _p16 = _p15._0;
      return Form_elm_builtin(_U.update(_p16,{y: _p16.y + y}));
   });
   var scale = F2(function (s,_p17) {
      var _p18 = _p17;
      var _p19 = _p18._0;
      return Form_elm_builtin(_U.update(_p19,
      {scale: _p19.scale * s}));
   });
   var rotate = F2(function (t,_p20) {
      var _p21 = _p20;
      var _p22 = _p21._0;
      return Form_elm_builtin(_U.update(_p22,
      {theta: _p22.theta + t}));
   });
   var alpha = F2(function (a,_p23) {
      var _p24 = _p23;
      return Form_elm_builtin(_U.update(_p24._0,{alpha: a}));
   });
   return _elm.Graphics.Collage.values = {_op: _op
                                         ,collage: collage
                                         ,toForm: toForm
                                         ,filled: filled
                                         ,textured: textured
                                         ,gradient: gradient
                                         ,outlined: outlined
                                         ,traced: traced
                                         ,text: text
                                         ,outlinedText: outlinedText
                                         ,move: move
                                         ,moveX: moveX
                                         ,moveY: moveY
                                         ,scale: scale
                                         ,rotate: rotate
                                         ,alpha: alpha
                                         ,group: group
                                         ,groupTransform: groupTransform
                                         ,rect: rect
                                         ,oval: oval
                                         ,square: square
                                         ,circle: circle
                                         ,ngon: ngon
                                         ,polygon: polygon
                                         ,segment: segment
                                         ,path: path
                                         ,solid: solid
                                         ,dashed: dashed
                                         ,dotted: dotted
                                         ,defaultLine: defaultLine
                                         ,LineStyle: LineStyle
                                         ,Flat: Flat
                                         ,Round: Round
                                         ,Padded: Padded
                                         ,Smooth: Smooth
                                         ,Sharp: Sharp
                                         ,Clipped: Clipped};
};
Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Utils.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary: F3(watchSummary)
	};
};

Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values) return _elm.Debug.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var _op = {};
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   return _elm.Debug.values = {_op: _op
                              ,log: log
                              ,crash: crash
                              ,watch: watch
                              ,watchSummary: watchSummary
                              ,trace: trace};
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values) return _elm.Result.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var _op = {};
   var toMaybe = function (result) {
      var _p0 = result;
      if (_p0.ctor === "Ok") {
            return $Maybe.Just(_p0._0);
         } else {
            return $Maybe.Nothing;
         }
   };
   var withDefault = F2(function (def,result) {
      var _p1 = result;
      if (_p1.ctor === "Ok") {
            return _p1._0;
         } else {
            return def;
         }
   });
   var Err = function (a) {    return {ctor: "Err",_0: a};};
   var andThen = F2(function (result,callback) {
      var _p2 = result;
      if (_p2.ctor === "Ok") {
            return callback(_p2._0);
         } else {
            return Err(_p2._0);
         }
   });
   var Ok = function (a) {    return {ctor: "Ok",_0: a};};
   var map = F2(function (func,ra) {
      var _p3 = ra;
      if (_p3.ctor === "Ok") {
            return Ok(func(_p3._0));
         } else {
            return Err(_p3._0);
         }
   });
   var map2 = F3(function (func,ra,rb) {
      var _p4 = {ctor: "_Tuple2",_0: ra,_1: rb};
      if (_p4._0.ctor === "Ok") {
            if (_p4._1.ctor === "Ok") {
                  return Ok(A2(func,_p4._0._0,_p4._1._0));
               } else {
                  return Err(_p4._1._0);
               }
         } else {
            return Err(_p4._0._0);
         }
   });
   var map3 = F4(function (func,ra,rb,rc) {
      var _p5 = {ctor: "_Tuple3",_0: ra,_1: rb,_2: rc};
      if (_p5._0.ctor === "Ok") {
            if (_p5._1.ctor === "Ok") {
                  if (_p5._2.ctor === "Ok") {
                        return Ok(A3(func,_p5._0._0,_p5._1._0,_p5._2._0));
                     } else {
                        return Err(_p5._2._0);
                     }
               } else {
                  return Err(_p5._1._0);
               }
         } else {
            return Err(_p5._0._0);
         }
   });
   var map4 = F5(function (func,ra,rb,rc,rd) {
      var _p6 = {ctor: "_Tuple4",_0: ra,_1: rb,_2: rc,_3: rd};
      if (_p6._0.ctor === "Ok") {
            if (_p6._1.ctor === "Ok") {
                  if (_p6._2.ctor === "Ok") {
                        if (_p6._3.ctor === "Ok") {
                              return Ok(A4(func,_p6._0._0,_p6._1._0,_p6._2._0,_p6._3._0));
                           } else {
                              return Err(_p6._3._0);
                           }
                     } else {
                        return Err(_p6._2._0);
                     }
               } else {
                  return Err(_p6._1._0);
               }
         } else {
            return Err(_p6._0._0);
         }
   });
   var map5 = F6(function (func,ra,rb,rc,rd,re) {
      var _p7 = {ctor: "_Tuple5"
                ,_0: ra
                ,_1: rb
                ,_2: rc
                ,_3: rd
                ,_4: re};
      if (_p7._0.ctor === "Ok") {
            if (_p7._1.ctor === "Ok") {
                  if (_p7._2.ctor === "Ok") {
                        if (_p7._3.ctor === "Ok") {
                              if (_p7._4.ctor === "Ok") {
                                    return Ok(A5(func,
                                    _p7._0._0,
                                    _p7._1._0,
                                    _p7._2._0,
                                    _p7._3._0,
                                    _p7._4._0));
                                 } else {
                                    return Err(_p7._4._0);
                                 }
                           } else {
                              return Err(_p7._3._0);
                           }
                     } else {
                        return Err(_p7._2._0);
                     }
               } else {
                  return Err(_p7._1._0);
               }
         } else {
            return Err(_p7._0._0);
         }
   });
   var formatError = F2(function (f,result) {
      var _p8 = result;
      if (_p8.ctor === "Ok") {
            return Ok(_p8._0);
         } else {
            return Err(f(_p8._0));
         }
   });
   var fromMaybe = F2(function (err,maybe) {
      var _p9 = maybe;
      if (_p9.ctor === "Just") {
            return Ok(_p9._0);
         } else {
            return Err(err);
         }
   });
   return _elm.Result.values = {_op: _op
                               ,withDefault: withDefault
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,andThen: andThen
                               ,toMaybe: toMaybe
                               ,fromMaybe: fromMaybe
                               ,formatError: formatError
                               ,Ok: Ok
                               ,Err: Err};
};
Elm.Native.Signal = {};

Elm.Native.Signal.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a, b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a, b, c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a, b, c, d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a, b, c, d, e]);
	}


	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.Task = {};

Elm.Native.Task.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			if (workQueue.length > 0)
			{
				var task = workQueue[0];

				setTimeout(function() {
					runTask(task, onComplete);
				}, 0);
			}
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values) return _elm.Task.values;
   var _U = Elm.Native.Utils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {
      return {ctor: "ThreadID",_0: a};
   };
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,task) {
      return A2(onError,
      task,
      function (err) {
         return fail(f(err));
      });
   });
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,taskA) {
      return A2(andThen,
      taskA,
      function (a) {
         return succeed(func(a));
      });
   });
   var map2 = F3(function (func,taskA,taskB) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return succeed(A2(func,a,b));
         });
      });
   });
   var map3 = F4(function (func,taskA,taskB,taskC) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return succeed(A3(func,a,b,c));
            });
         });
      });
   });
   var map4 = F5(function (func,taskA,taskB,taskC,taskD) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return A2(andThen,
               taskD,
               function (d) {
                  return succeed(A4(func,a,b,c,d));
               });
            });
         });
      });
   });
   var map5 = F6(function (func,taskA,taskB,taskC,taskD,taskE) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return A2(andThen,
               taskD,
               function (d) {
                  return A2(andThen,
                  taskE,
                  function (e) {
                     return succeed(A5(func,a,b,c,d,e));
                  });
               });
            });
         });
      });
   });
   var andMap = F2(function (taskFunc,taskValue) {
      return A2(andThen,
      taskFunc,
      function (func) {
         return A2(andThen,
         taskValue,
         function (value) {
            return succeed(func(value));
         });
      });
   });
   var sequence = function (tasks) {
      var _p0 = tasks;
      if (_p0.ctor === "[]") {
            return succeed(_U.list([]));
         } else {
            return A3(map2,
            F2(function (x,y) {    return A2($List._op["::"],x,y);}),
            _p0._0,
            sequence(_p0._1));
         }
   };
   var toMaybe = function (task) {
      return A2(onError,
      A2(map,$Maybe.Just,task),
      function (_p1) {
         return succeed($Maybe.Nothing);
      });
   };
   var fromMaybe = F2(function ($default,maybe) {
      var _p2 = maybe;
      if (_p2.ctor === "Just") {
            return succeed(_p2._0);
         } else {
            return fail($default);
         }
   });
   var toResult = function (task) {
      return A2(onError,
      A2(map,$Result.Ok,task),
      function (msg) {
         return succeed($Result.Err(msg));
      });
   };
   var fromResult = function (result) {
      var _p3 = result;
      if (_p3.ctor === "Ok") {
            return succeed(_p3._0);
         } else {
            return fail(_p3._0);
         }
   };
   var Task = {ctor: "Task"};
   return _elm.Task.values = {_op: _op
                             ,succeed: succeed
                             ,fail: fail
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,andMap: andMap
                             ,sequence: sequence
                             ,andThen: andThen
                             ,onError: onError
                             ,mapError: mapError
                             ,toMaybe: toMaybe
                             ,fromMaybe: fromMaybe
                             ,toResult: toResult
                             ,fromResult: fromResult
                             ,spawn: spawn
                             ,sleep: sleep};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values) return _elm.Signal.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var send = F2(function (_p0,value) {
      var _p1 = _p0;
      return A2($Task.onError,
      _p1._0(value),
      function (_p2) {
         return $Task.succeed({ctor: "_Tuple0"});
      });
   });
   var Message = function (a) {
      return {ctor: "Message",_0: a};
   };
   var message = F2(function (_p3,value) {
      var _p4 = _p3;
      return Message(_p4._0(value));
   });
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {
      return {ctor: "Address",_0: a};
   };
   var forwardTo = F2(function (_p5,f) {
      var _p6 = _p5;
      return Address(function (x) {    return _p6._0(f(x));});
   });
   var Mailbox = F2(function (a,b) {
      return {address: a,signal: b};
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,base,signal) {
      return A3(filterMap,
      function (value) {
         return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;
      },
      base,
      signal);
   });
   var merge = F2(function (left,right) {
      return A3($Native$Signal.genericMerge,
      $Basics.always,
      left,
      right);
   });
   var mergeMany = function (signalList) {
      var _p7 = $List.reverse(signalList);
      if (_p7.ctor === "[]") {
            return _U.crashCase("Signal",
            {start: {line: 184,column: 3},end: {line: 189,column: 40}},
            _p7)("mergeMany was given an empty list!");
         } else {
            return A3($List.foldl,merge,_p7._0,_p7._1);
         }
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   var map = $Native$Signal.map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   return _elm.Signal.values = {_op: _op
                               ,merge: merge
                               ,mergeMany: mergeMany
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,constant: constant
                               ,dropRepeats: dropRepeats
                               ,filter: filter
                               ,filterMap: filterMap
                               ,sampleOn: sampleOn
                               ,foldp: foldp
                               ,mailbox: mailbox
                               ,send: send
                               ,message: message
                               ,forwardTo: forwardTo
                               ,Mailbox: Mailbox};
};
Elm.Native.Lazy = {};
Elm.Native.Lazy.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Lazy = localRuntime.Native.Lazy || {};
    if (localRuntime.Native.Lazy.values) {
        return localRuntime.Native.Lazy.values;
    }

    function memoize(thunk) {
        var value;
        var isForced = false;
        return function(tuple0) {
            if (!isForced) {
                value = thunk(tuple0);
                isForced = true;
            }
            return value;
        };
    }

    return localRuntime.Native.Lazy.values = {
        memoize: memoize
    };
};

Elm.Lazy = Elm.Lazy || {};
Elm.Lazy.make = function (_elm) {
   "use strict";
   _elm.Lazy = _elm.Lazy || {};
   if (_elm.Lazy.values) return _elm.Lazy.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Lazy = Elm.Native.Lazy.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var force = function (_p0) {
      var _p1 = _p0;
      return _p1._0({ctor: "_Tuple0"});
   };
   var Lazy = function (a) {    return {ctor: "Lazy",_0: a};};
   var lazy = function (thunk) {
      return Lazy($Native$Lazy.memoize(thunk));
   };
   var map = F2(function (f,a) {
      return lazy(function (_p2) {
         var _p3 = _p2;
         return f(force(a));
      });
   });
   var map2 = F3(function (f,a,b) {
      return lazy(function (_p4) {
         var _p5 = _p4;
         return A2(f,force(a),force(b));
      });
   });
   var map3 = F4(function (f,a,b,c) {
      return lazy(function (_p6) {
         var _p7 = _p6;
         return A3(f,force(a),force(b),force(c));
      });
   });
   var map4 = F5(function (f,a,b,c,d) {
      return lazy(function (_p8) {
         var _p9 = _p8;
         return A4(f,force(a),force(b),force(c),force(d));
      });
   });
   var map5 = F6(function (f,a,b,c,d,e) {
      return lazy(function (_p10) {
         var _p11 = _p10;
         return A5(f,force(a),force(b),force(c),force(d),force(e));
      });
   });
   var apply = F2(function (f,x) {
      return lazy(function (_p12) {
         var _p13 = _p12;
         return A2(force,f,force(x));
      });
   });
   var andThen = F2(function (a,callback) {
      return lazy(function (_p14) {
         var _p15 = _p14;
         return force(callback(force(a)));
      });
   });
   return _elm.Lazy.values = {_op: _op
                             ,force: force
                             ,lazy: lazy
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,apply: apply
                             ,andThen: andThen};
};
Elm.Native.String = {};

Elm.Native.String.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr, str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd = str[0];
		if (hd)
		{
			return Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)));
		}
		return Maybe.Nothing;
	}
	function append(a, b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f, str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred, str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f, b, str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f, b, str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}
	function slice(start, end, str)
	{
		return str.slice(start, end);
	}
	function left(n, str)
	{
		return n < 1 ? '' : str.slice(0, n);
	}
	function right(n, str)
	{
		return n < 1 ? '' : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0, -n);
	}
	function pad(n, chr, str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
	}
	function padRight(n, chr, str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n, chr, str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.replace(/^\s+/, '');
	}
	function trimRight(str)
	{
		return str.replace(/\s+$/, '');
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode: function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode: function(c) { return c.charCodeAt(0); },
		toUpper: function(c) { return Utils.chr(c.toUpperCase()); },
		toLower: function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper: function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower: function(c) { return Utils.chr(c.toLocaleLowerCase()); }
	};
};

Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values) return _elm.Char.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Native$Char = Elm.Native.Char.make(_elm);
   var _op = {};
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,high,$char) {
      var code = toCode($char);
      return _U.cmp(code,toCode(low)) > -1 && _U.cmp(code,
      toCode(high)) < 1;
   });
   var isUpper = A2(isBetween,_U.chr("A"),_U.chr("Z"));
   var isLower = A2(isBetween,_U.chr("a"),_U.chr("z"));
   var isDigit = A2(isBetween,_U.chr("0"),_U.chr("9"));
   var isOctDigit = A2(isBetween,_U.chr("0"),_U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,
      _U.chr("a"),
      _U.chr("f"),
      $char) || A3(isBetween,_U.chr("A"),_U.chr("F"),$char));
   };
   return _elm.Char.values = {_op: _op
                             ,isUpper: isUpper
                             ,isLower: isLower
                             ,isDigit: isDigit
                             ,isOctDigit: isOctDigit
                             ,isHexDigit: isHexDigit
                             ,toUpper: toUpper
                             ,toLower: toLower
                             ,toLocaleUpper: toLocaleUpper
                             ,toLocaleLower: toLocaleLower
                             ,toCode: toCode
                             ,fromCode: fromCode};
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values) return _elm.String.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {    return A2(cons,$char,"");};
   var isEmpty = $Native$String.isEmpty;
   return _elm.String.values = {_op: _op
                               ,isEmpty: isEmpty
                               ,length: length
                               ,reverse: reverse
                               ,repeat: repeat
                               ,cons: cons
                               ,uncons: uncons
                               ,fromChar: fromChar
                               ,append: append
                               ,concat: concat
                               ,split: split
                               ,join: join
                               ,words: words
                               ,lines: lines
                               ,slice: slice
                               ,left: left
                               ,right: right
                               ,dropLeft: dropLeft
                               ,dropRight: dropRight
                               ,contains: contains
                               ,startsWith: startsWith
                               ,endsWith: endsWith
                               ,indexes: indexes
                               ,indices: indices
                               ,toInt: toInt
                               ,toFloat: toFloat
                               ,toList: toList
                               ,fromList: fromList
                               ,toUpper: toUpper
                               ,toLower: toLower
                               ,pad: pad
                               ,padLeft: padLeft
                               ,padRight: padRight
                               ,trim: trim
                               ,trimLeft: trimLeft
                               ,trimRight: trimRight
                               ,map: map
                               ,filter: filter
                               ,foldl: foldl
                               ,foldr: foldr
                               ,any: any
                               ,all: all};
};
Elm.Native.Regex = {};
Elm.Native.Regex.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Regex = localRuntime.Native.Regex || {};
	if (localRuntime.Native.Regex.values)
	{
		return localRuntime.Native.Regex.values;
	}
	if ('values' in Elm.Native.Regex)
	{
		return localRuntime.Native.Regex.values = Elm.Native.Regex.values;
	}

	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);

	function escape(str)
	{
		return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
	}
	function caseInsensitive(re)
	{
		return new RegExp(re.source, 'gi');
	}
	function regex(raw)
	{
		return new RegExp(raw, 'g');
	}

	function contains(re, string)
	{
		return string.match(re) !== null;
	}

	function find(n, re, str)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		var out = [];
		var number = 0;
		var string = str;
		var lastIndex = re.lastIndex;
		var prevLastIndex = -1;
		var result;
		while (number++ < n && (result = re.exec(string)))
		{
			if (prevLastIndex === re.lastIndex) break;
			var i = result.length - 1;
			var subs = new Array(i);
			while (i > 0)
			{
				var submatch = result[i];
				subs[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			out.push({
				match: result[0],
				submatches: List.fromArray(subs),
				index: result.index,
				number: number
			});
			prevLastIndex = re.lastIndex;
		}
		re.lastIndex = lastIndex;
		return List.fromArray(out);
	}

	function replace(n, re, replacer, string)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		var count = 0;
		function jsReplacer(match)
		{
			if (count++ >= n)
			{
				return match;
			}
			var i = arguments.length - 3;
			var submatches = new Array(i);
			while (i > 0)
			{
				var submatch = arguments[i];
				submatches[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			return replacer({
				match: match,
				submatches: List.fromArray(submatches),
				index: arguments[i - 1],
				number: count
			});
		}
		return string.replace(re, jsReplacer);
	}

	function split(n, re, str)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		if (n === Infinity)
		{
			return List.fromArray(str.split(re));
		}
		var string = str;
		var result;
		var out = [];
		var start = re.lastIndex;
		while (n--)
		{
			if (!(result = re.exec(string))) break;
			out.push(string.slice(start, result.index));
			start = re.lastIndex;
		}
		out.push(string.slice(start));
		return List.fromArray(out);
	}

	return Elm.Native.Regex.values = {
		regex: regex,
		caseInsensitive: caseInsensitive,
		escape: escape,

		contains: F2(contains),
		find: F3(find),
		replace: F4(replace),
		split: F3(split)
	};
};

Elm.Regex = Elm.Regex || {};
Elm.Regex.make = function (_elm) {
   "use strict";
   _elm.Regex = _elm.Regex || {};
   if (_elm.Regex.values) return _elm.Regex.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Regex = Elm.Native.Regex.make(_elm);
   var _op = {};
   var split = $Native$Regex.split;
   var replace = $Native$Regex.replace;
   var find = $Native$Regex.find;
   var AtMost = function (a) {    return {ctor: "AtMost",_0: a};};
   var All = {ctor: "All"};
   var Match = F4(function (a,b,c,d) {
      return {match: a,submatches: b,index: c,number: d};
   });
   var contains = $Native$Regex.contains;
   var caseInsensitive = $Native$Regex.caseInsensitive;
   var regex = $Native$Regex.regex;
   var escape = $Native$Regex.escape;
   var Regex = {ctor: "Regex"};
   return _elm.Regex.values = {_op: _op
                              ,regex: regex
                              ,escape: escape
                              ,caseInsensitive: caseInsensitive
                              ,contains: contains
                              ,find: find
                              ,replace: replace
                              ,split: split
                              ,Match: Match
                              ,All: All
                              ,AtMost: AtMost};
};
Elm.Combine = Elm.Combine || {};
Elm.Combine.make = function (_elm) {
   "use strict";
   _elm.Combine = _elm.Combine || {};
   if (_elm.Combine.values) return _elm.Combine.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Lazy = Elm.Lazy.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var app = function (p) {
      var _p0 = p;
      if (_p0.ctor === "Parser") {
            return _p0._0;
         } else {
            return $Lazy.force(_p0._0);
         }
   };
   var parse = F2(function (p,input) {
      return A2(app,p,{input: input,position: 0});
   });
   var RecursiveParser = function (a) {
      return {ctor: "RecursiveParser",_0: a};
   };
   var rec = function (t) {
      return RecursiveParser($Lazy.lazy(function (_p1) {
         var _p2 = _p1;
         return app(t({ctor: "_Tuple0"}));
      }));
   };
   var Parser = function (a) {    return {ctor: "Parser",_0: a};};
   var primitive = Parser;
   var bimap = F3(function (fok,ferr,p) {
      return Parser(function (cx) {
         var _p3 = A2(app,p,cx);
         if (_p3._0.ctor === "Ok") {
               return {ctor: "_Tuple2"
                      ,_0: $Result.Ok(fok(_p3._0._0))
                      ,_1: _p3._1};
            } else {
               return {ctor: "_Tuple2"
                      ,_0: $Result.Err(ferr(_p3._0._0))
                      ,_1: _p3._1};
            }
      });
   });
   var map = F2(function (f,p) {
      return A3(bimap,f,$Basics.identity,p);
   });
   var mapError = bimap($Basics.identity);
   var andThen = F2(function (p,f) {
      return Parser(function (cx) {
         var _p4 = A2(app,p,cx);
         if (_p4._0.ctor === "Ok") {
               return A2(app,f(_p4._0._0),_p4._1);
            } else {
               return {ctor: "_Tuple2"
                      ,_0: $Result.Err(_p4._0._0)
                      ,_1: _p4._1};
            }
      });
   });
   var fail = function (ms) {
      return Parser(function (cx) {
         return {ctor: "_Tuple2",_0: $Result.Err(ms),_1: cx};
      });
   };
   var succeed = function (r) {
      return Parser(function (cx) {
         return {ctor: "_Tuple2",_0: $Result.Ok(r),_1: cx};
      });
   };
   var andMap = F2(function (lp,rp) {
      return A2(andThen,
      lp,
      function (f) {
         return A2(andThen,
         rp,
         function (x) {
            return succeed(f(x));
         });
      });
   });
   var between = F3(function (lp,rp,p) {
      return A2(andMap,
      A2(andMap,
      A2(map,
      $Basics.flip(function (_p5) {
         return $Basics.always($Basics.always(_p5));
      }),
      lp),
      p),
      rp);
   });
   var skip = function (p) {
      return A2(andThen,
      p,
      $Basics.always(succeed({ctor: "_Tuple0"})));
   };
   var count = F2(function (n,p) {
      var accumulate = F2(function (x,acc) {
         return _U.cmp(x,
         0) < 1 ? succeed($List.reverse(acc)) : A2(andThen,
         p,
         function (res) {
            return A2(accumulate,x - 1,A2($List._op["::"],res,acc));
         });
      });
      return A2(accumulate,n,_U.list([]));
   });
   var string = function (s) {
      return Parser(function (cx) {
         if (A2($String.startsWith,s,cx.input)) {
               var len = $String.length(s);
               var rem = A2($String.dropLeft,len,cx.input);
               var pos = cx.position + len;
               return {ctor: "_Tuple2"
                      ,_0: $Result.Ok(s)
                      ,_1: _U.update(cx,{input: rem,position: pos})};
            } else return {ctor: "_Tuple2"
                          ,_0: $Result.Err(_U.list([A2($Basics._op["++"],
                          "expected ",
                          $Basics.toString(s))]))
                          ,_1: cx};
      });
   };
   var parens = A2(between,string("("),string(")"));
   var braces = A2(between,string("{"),string("}"));
   var brackets = A2(between,string("["),string("]"));
   var regex = function (pattern) {
      var pattern$ = A2($String.startsWith,
      "^",
      pattern) ? pattern : A2($Basics._op["++"],"^",pattern);
      return Parser(function (cx) {
         var _p6 = A3($Regex.find,
         $Regex.AtMost(1),
         $Regex.regex(pattern$),
         cx.input);
         if (_p6.ctor === "::" && _p6._1.ctor === "[]") {
               var _p7 = _p6._0;
               var len = $String.length(_p7.match);
               var rem = A2($String.dropLeft,len,cx.input);
               var pos = cx.position + len;
               return {ctor: "_Tuple2"
                      ,_0: $Result.Ok(_p7.match)
                      ,_1: _U.update(cx,{input: rem,position: pos})};
            } else {
               return {ctor: "_Tuple2"
                      ,_0: $Result.Err(_U.list([A2($Basics._op["++"],
                      "expected input matching Regexp /",
                      A2($Basics._op["++"],pattern$,"/"))]))
                      ,_1: cx};
            }
      });
   };
   var $while = function (pred) {
      var accumulate = F2(function (acc,cx) {
         accumulate: while (true) {
            var _p8 = $String.uncons(cx.input);
            if (_p8.ctor === "Just") {
                  var _p9 = _p8._0._0;
                  if (pred(_p9)) {
                        var pos = cx.position + 1;
                        var c = A2($String.cons,_p9,"");
                        var _v6 = A2($Basics._op["++"],acc,c),
                        _v7 = _U.update(cx,{input: _p8._0._1,position: pos});
                        acc = _v6;
                        cx = _v7;
                        continue accumulate;
                     } else return {ctor: "_Tuple2",_0: acc,_1: cx};
               } else {
                  return {ctor: "_Tuple2",_0: acc,_1: cx};
               }
         }
      });
      return Parser(function (cx) {
         var _p10 = A2(accumulate,"",cx);
         var res = _p10._0;
         var cx$ = _p10._1;
         return {ctor: "_Tuple2",_0: $Result.Ok(res),_1: cx$};
      });
   };
   var end = Parser(function (cx) {
      return _U.eq(cx.input,"") ? {ctor: "_Tuple2"
                                  ,_0: $Result.Ok({ctor: "_Tuple0"})
                                  ,_1: cx} : {ctor: "_Tuple2"
                                             ,_0: $Result.Err(_U.list(["expected end of input"]))
                                             ,_1: cx};
   });
   var or = F2(function (lp,rp) {
      return Parser(function (cx) {
         var res = A2(app,lp,cx);
         var _p11 = res;
         if (_p11._0.ctor === "Ok") {
               return res;
            } else {
               var res$ = A2(app,rp,cx);
               var _p12 = res$;
               if (_p12._0.ctor === "Ok") {
                     return res$;
                  } else {
                     return {ctor: "_Tuple2"
                            ,_0: $Result.Err(A2($Basics._op["++"],_p11._0._0,_p12._0._0))
                            ,_1: cx};
                  }
            }
      });
   });
   var choice = function (xs) {
      return A3($List.foldr,or,fail(_U.list([])),xs);
   };
   var optional = F2(function (res,p) {
      return A2(or,p,succeed(res));
   });
   var chainl = F2(function (p,op) {
      var accumulate = function (x) {
         return A2(or,
         A2(andThen,
         op,
         function (f) {
            return A2(andThen,
            p,
            function (y) {
               return accumulate(A2(f,x,y));
            });
         }),
         succeed(x));
      };
      return A2(andThen,p,accumulate);
   });
   var chainr = F2(function (p,op) {
      var accumulate = function (x) {
         return A2(or,
         A2(andThen,
         op,
         function (f) {
            return A2(andThen,
            A2(andThen,p,accumulate),
            function (y) {
               return succeed(A2(f,x,y));
            });
         }),
         succeed(x));
      };
      return A2(andThen,p,accumulate);
   });
   var maybe = function (p) {
      return Parser(function (cx) {
         var _p13 = A2(app,p,cx);
         if (_p13.ctor === "_Tuple2" && _p13._0.ctor === "Ok") {
               return {ctor: "_Tuple2"
                      ,_0: $Result.Ok($Maybe.Just(_p13._0._0))
                      ,_1: _p13._1};
            } else {
               return {ctor: "_Tuple2"
                      ,_0: $Result.Ok($Maybe.Nothing)
                      ,_1: cx};
            }
      });
   };
   var many = function (p) {
      var accumulate = F2(function (acc,cx) {
         accumulate: while (true) {
            var _p14 = A2(app,p,cx);
            if (_p14.ctor === "_Tuple2" && _p14._0.ctor === "Ok") {
                  var _p15 = _p14._1;
                  if (_U.eq(cx,_p15)) return {ctor: "_Tuple2"
                                             ,_0: $List.reverse(acc)
                                             ,_1: cx}; else {
                        var _v12 = A2($List._op["::"],_p14._0._0,acc),_v13 = _p15;
                        acc = _v12;
                        cx = _v13;
                        continue accumulate;
                     }
               } else {
                  return {ctor: "_Tuple2",_0: $List.reverse(acc),_1: cx};
               }
         }
      });
      return Parser(function (cx) {
         var _p16 = A2(accumulate,_U.list([]),cx);
         var res = _p16._0;
         var cx$ = _p16._1;
         return {ctor: "_Tuple2",_0: $Result.Ok(res),_1: cx$};
      });
   };
   var many1 = function (p) {
      return A2(andMap,
      A2(map,
      F2(function (x,y) {    return A2($List._op["::"],x,y);}),
      p),
      many(p));
   };
   var skipMany1 = function (p) {
      return A2(andThen,
      many1(skip(p)),
      $Basics.always(succeed({ctor: "_Tuple0"})));
   };
   var sepBy1 = F2(function (sep,p) {
      return A2(andMap,
      A2(map,
      F2(function (x,y) {    return A2($List._op["::"],x,y);}),
      p),
      many(A2(andMap,A2(map,$Basics.flip($Basics.always),sep),p)));
   });
   var sepBy = F2(function (sep,p) {
      return A2(or,A2(sepBy1,sep,p),succeed(_U.list([])));
   });
   var sepEndBy1 = F2(function (sep,p) {
      return A2(andMap,
      A2(map,$Basics.always,A2(sepBy1,sep,p)),
      maybe(sep));
   });
   var sepEndBy = F2(function (sep,p) {
      return A2(or,A2(sepEndBy1,sep,p),succeed(_U.list([])));
   });
   var skipMany = function (p) {
      return A2(andThen,
      many(skip(p)),
      $Basics.always(succeed({ctor: "_Tuple0"})));
   };
   var manyTill = F2(function (p,end) {
      var accumulate = F2(function (acc,cx) {
         accumulate: while (true) {
            var _p17 = A2(app,end,cx);
            if (_p17._0.ctor === "Ok") {
                  return {ctor: "_Tuple2"
                         ,_0: $Result.Ok($List.reverse(acc))
                         ,_1: _p17._1};
               } else {
                  var _p18 = A2(app,p,cx);
                  if (_p18.ctor === "_Tuple2" && _p18._0.ctor === "Ok") {
                        var _v16 = A2($List._op["::"],_p18._0._0,acc),_v17 = _p18._1;
                        acc = _v16;
                        cx = _v17;
                        continue accumulate;
                     } else {
                        return {ctor: "_Tuple2"
                               ,_0: $Result.Err(_p17._0._0)
                               ,_1: _p17._1};
                     }
               }
         }
      });
      return Parser(accumulate(_U.list([])));
   });
   var Context = F2(function (a,b) {
      return {input: a,position: b};
   });
   return _elm.Combine.values = {_op: _op
                                ,primitive: primitive
                                ,parse: parse
                                ,app: app
                                ,rec: rec
                                ,bimap: bimap
                                ,map: map
                                ,mapError: mapError
                                ,andThen: andThen
                                ,andMap: andMap
                                ,fail: fail
                                ,succeed: succeed
                                ,string: string
                                ,regex: regex
                                ,$while: $while
                                ,end: end
                                ,or: or
                                ,choice: choice
                                ,optional: optional
                                ,maybe: maybe
                                ,many: many
                                ,many1: many1
                                ,manyTill: manyTill
                                ,sepBy: sepBy
                                ,sepBy1: sepBy1
                                ,sepEndBy: sepEndBy
                                ,sepEndBy1: sepEndBy1
                                ,skip: skip
                                ,skipMany: skipMany
                                ,skipMany1: skipMany1
                                ,chainl: chainl
                                ,chainr: chainr
                                ,count: count
                                ,between: between
                                ,parens: parens
                                ,braces: braces
                                ,brackets: brackets
                                ,Context: Context};
};
Elm.Combine = Elm.Combine || {};
Elm.Combine.Infix = Elm.Combine.Infix || {};
Elm.Combine.Infix.make = function (_elm) {
   "use strict";
   _elm.Combine = _elm.Combine || {};
   _elm.Combine.Infix = _elm.Combine.Infix || {};
   if (_elm.Combine.Infix.values) return _elm.Combine.Infix.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Combine = Elm.Combine.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   _op["<|>"] = $Combine.or;
   _op["*>"] = F2(function (lp,rp) {
      return A2($Combine.andMap,
      A2($Combine.map,$Basics.flip($Basics.always),lp),
      rp);
   });
   _op["<*"] = F2(function (lp,rp) {
      return A2($Combine.andMap,
      A2($Combine.map,$Basics.always,lp),
      rp);
   });
   _op["<?>"] = F2(function (p,m) {
      return A2($Combine.mapError,
      function (_p0) {
         return _U.list([m]);
      },
      p);
   });
   _op["<$"] = function (res) {
      return $Combine.map(function (_p1) {    return res;});
   };
   _op["<*>"] = $Combine.andMap;
   _op["<$>"] = $Combine.map;
   return _elm.Combine.Infix.values = {_op: _op};
};
Elm.Combine = Elm.Combine || {};
Elm.Combine.Char = Elm.Combine.Char || {};
Elm.Combine.Char.make = function (_elm) {
   "use strict";
   _elm.Combine = _elm.Combine || {};
   _elm.Combine.Char = _elm.Combine.Char || {};
   if (_elm.Combine.Char.values) return _elm.Combine.Char.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Combine = Elm.Combine.make(_elm),
   $Combine$Infix = Elm.Combine.Infix.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var crlf = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$"],
   _U.chr("\n"),
   $Combine.regex("\r\n")),
   "expected crlf");
   var satisfy = function (pred) {
      return $Combine.primitive(function (cx) {
         var message = "could not satisfy predicate";
         var _p0 = $String.uncons(cx.input);
         if (_p0.ctor === "Just") {
               var _p1 = _p0._0._0;
               return pred(_p1) ? {ctor: "_Tuple2"
                                  ,_0: $Result.Ok(_p1)
                                  ,_1: _U.update(cx,
                                  {input: _p0._0._1
                                  ,position: cx.position + 1})} : {ctor: "_Tuple2"
                                                                  ,_0: $Result.Err(_U.list([message]))
                                                                  ,_1: cx};
            } else {
               return {ctor: "_Tuple2"
                      ,_0: $Result.Err(_U.list([message]))
                      ,_1: cx};
            }
      });
   };
   var $char = function (c) {
      return A2($Combine$Infix._op["<?>"],
      satisfy(F2(function (x,y) {    return _U.eq(x,y);})(c)),
      A2($Basics._op["++"],"expected ",$Basics.toString(c)));
   };
   var anyChar = A2($Combine$Infix._op["<?>"],
   satisfy($Basics.always(true)),
   "expected any character");
   var oneOf = function (cs) {
      return A2($Combine$Infix._op["<?>"],
      satisfy(A2($Basics.flip,$List.member,cs)),
      A2($Basics._op["++"],"expected one of ",$Basics.toString(cs)));
   };
   var noneOf = function (cs) {
      return A2($Combine$Infix._op["<?>"],
      satisfy(function (_p2) {
         return $Basics.not(A3($Basics.flip,$List.member,cs,_p2));
      }),
      A2($Basics._op["++"],"expected none of ",$Basics.toString(cs)));
   };
   var space = A2($Combine$Infix._op["<?>"],
   satisfy(F2(function (x,y) {
      return _U.eq(x,y);
   })(_U.chr(" "))),
   "expected space");
   var tab = A2($Combine$Infix._op["<?>"],
   satisfy(F2(function (x,y) {
      return _U.eq(x,y);
   })(_U.chr("\t"))),
   "expected tab");
   var newline = A2($Combine$Infix._op["<?>"],
   satisfy(F2(function (x,y) {
      return _U.eq(x,y);
   })(_U.chr("\n"))),
   "expected newline");
   var eol = A2($Combine$Infix._op["<|>"],newline,crlf);
   var lower = A2($Combine$Infix._op["<?>"],
   satisfy($Char.isLower),
   "expected a lowercase character");
   var upper = A2($Combine$Infix._op["<?>"],
   satisfy($Char.isUpper),
   "expected an uppercase character");
   var digit = A2($Combine$Infix._op["<?>"],
   satisfy($Char.isDigit),
   "expected a digit");
   var octDigit = A2($Combine$Infix._op["<?>"],
   satisfy($Char.isOctDigit),
   "expected an octal digit");
   var hexDigit = A2($Combine$Infix._op["<?>"],
   satisfy($Char.isHexDigit),
   "expected a hexadecimal digit");
   return _elm.Combine.Char.values = {_op: _op
                                     ,satisfy: satisfy
                                     ,$char: $char
                                     ,anyChar: anyChar
                                     ,oneOf: oneOf
                                     ,noneOf: noneOf
                                     ,space: space
                                     ,tab: tab
                                     ,newline: newline
                                     ,crlf: crlf
                                     ,eol: eol
                                     ,lower: lower
                                     ,upper: upper
                                     ,digit: digit
                                     ,octDigit: octDigit
                                     ,hexDigit: hexDigit};
};
Elm.Combine = Elm.Combine || {};
Elm.Combine.Num = Elm.Combine.Num || {};
Elm.Combine.Num.make = function (_elm) {
   "use strict";
   _elm.Combine = _elm.Combine || {};
   _elm.Combine.Num = _elm.Combine.Num || {};
   if (_elm.Combine.Num.values) return _elm.Combine.Num.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Combine = Elm.Combine.make(_elm),
   $Combine$Char = Elm.Combine.Char.make(_elm),
   $Combine$Infix = Elm.Combine.Infix.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var digit = function () {
      var toDigit = function (c) {
         return $Char.toCode(c) - $Char.toCode(_U.chr("0"));
      };
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],toDigit,$Combine$Char.digit),
      "expected a digit");
   }();
   var sign = A2($Combine.optional,
   1,
   $Combine.choice(_U.list([A2($Combine$Infix._op["<$"],
                           1,
                           $Combine.string("+"))
                           ,A2($Combine$Infix._op["<$"],-1,$Combine.string("-"))])));
   var unwrap = F2(function (f,s) {
      var _p0 = f(s);
      if (_p0.ctor === "Ok") {
            return _p0._0;
         } else {
            return _U.crashCase("Combine.Num",
            {start: {line: 19,column: 3},end: {line: 24,column: 73}},
            _p0)(A2($Basics._op["++"],
            "impossible state in Combine.Num.unwrap: ",
            $Basics.toString(_p0._0)));
         }
   });
   var toInt = unwrap($String.toInt);
   var $int = A2($Combine$Infix._op["<?>"],
   A2($Combine.andMap,
   A2($Combine.map,F2(function (x,y) {    return x * y;}),sign),
   A2($Combine$Infix._op["<$>"],
   toInt,
   $Combine.regex("(0|[1-9][0-9]*)"))),
   "expected an integer");
   var toFloat = unwrap($String.toFloat);
   var $float = A2($Combine$Infix._op["<?>"],
   A2($Combine.andMap,
   A2($Combine.map,
   function (_p2) {
      return F2(function (x,y) {
         return x * y;
      })($Basics.toFloat(_p2));
   },
   sign),
   A2($Combine$Infix._op["<$>"],
   toFloat,
   $Combine.regex("(0|[1-9][0-9]*)(\\.[0-9]+)"))),
   "expected a float");
   return _elm.Combine.Num.values = {_op: _op
                                    ,sign: sign
                                    ,digit: digit
                                    ,$int: $int
                                    ,$float: $float};
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values) return _elm.Dict.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var foldr = F3(function (f,acc,t) {
      foldr: while (true) {
         var _p0 = t;
         if (_p0.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v1 = f,
               _v2 = A3(f,_p0._1,_p0._2,A3(foldr,f,acc,_p0._4)),
               _v3 = _p0._3;
               f = _v1;
               acc = _v2;
               t = _v3;
               continue foldr;
            }
      }
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,value,keyList) {
         return A2($List._op["::"],key,keyList);
      }),
      _U.list([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,value,valueList) {
         return A2($List._op["::"],value,valueList);
      }),
      _U.list([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2",_0: key,_1: value},
         list);
      }),
      _U.list([]),
      dict);
   };
   var foldl = F3(function (f,acc,dict) {
      foldl: while (true) {
         var _p1 = dict;
         if (_p1.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v5 = f,
               _v6 = A3(f,_p1._1,_p1._2,A3(foldl,f,acc,_p1._3)),
               _v7 = _p1._4;
               f = _v5;
               acc = _v6;
               dict = _v7;
               continue foldl;
            }
      }
   });
   var reportRemBug = F4(function (msg,c,lgot,rgot) {
      return $Native$Debug.crash($String.concat(_U.list(["Internal red-black tree invariant violated, expected "
                                                        ,msg
                                                        ," and got "
                                                        ,$Basics.toString(c)
                                                        ,"/"
                                                        ,lgot
                                                        ,"/"
                                                        ,rgot
                                                        ,"\nPlease report this bug to <https://github.com/elm-lang/core/issues>"])));
   });
   var isBBlack = function (dict) {
      var _p2 = dict;
      _v8_2: do {
         if (_p2.ctor === "RBNode_elm_builtin") {
               if (_p2._0.ctor === "BBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            } else {
               if (_p2._0.ctor === "LBBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            }
      } while (false);
      return false;
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var sizeHelp = F2(function (n,dict) {
      sizeHelp: while (true) {
         var _p3 = dict;
         if (_p3.ctor === "RBEmpty_elm_builtin") {
               return n;
            } else {
               var _v10 = A2(sizeHelp,n + 1,_p3._4),_v11 = _p3._3;
               n = _v10;
               dict = _v11;
               continue sizeHelp;
            }
      }
   });
   var size = function (dict) {    return A2(sizeHelp,0,dict);};
   var get = F2(function (targetKey,dict) {
      get: while (true) {
         var _p4 = dict;
         if (_p4.ctor === "RBEmpty_elm_builtin") {
               return $Maybe.Nothing;
            } else {
               var _p5 = A2($Basics.compare,targetKey,_p4._1);
               switch (_p5.ctor)
               {case "LT": var _v14 = targetKey,_v15 = _p4._3;
                    targetKey = _v14;
                    dict = _v15;
                    continue get;
                  case "EQ": return $Maybe.Just(_p4._2);
                  default: var _v16 = targetKey,_v17 = _p4._4;
                    targetKey = _v16;
                    dict = _v17;
                    continue get;}
            }
      }
   });
   var member = F2(function (key,dict) {
      var _p6 = A2(get,key,dict);
      if (_p6.ctor === "Just") {
            return true;
         } else {
            return false;
         }
   });
   var maxWithDefault = F3(function (k,v,r) {
      maxWithDefault: while (true) {
         var _p7 = r;
         if (_p7.ctor === "RBEmpty_elm_builtin") {
               return {ctor: "_Tuple2",_0: k,_1: v};
            } else {
               var _v20 = _p7._1,_v21 = _p7._2,_v22 = _p7._4;
               k = _v20;
               v = _v21;
               r = _v22;
               continue maxWithDefault;
            }
      }
   });
   var RBEmpty_elm_builtin = function (a) {
      return {ctor: "RBEmpty_elm_builtin",_0: a};
   };
   var RBNode_elm_builtin = F5(function (a,b,c,d,e) {
      return {ctor: "RBNode_elm_builtin"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty_elm_builtin(LBlack);
   var isEmpty = function (dict) {    return _U.eq(dict,empty);};
   var map = F2(function (f,dict) {
      var _p8 = dict;
      if (_p8.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            var _p9 = _p8._1;
            return A5(RBNode_elm_builtin,
            _p8._0,
            _p9,
            A2(f,_p9,_p8._2),
            A2(map,f,_p8._3),
            A2(map,f,_p8._4));
         }
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      var _p10 = dict;
      if (_p10.ctor === "RBNode_elm_builtin" && _p10._0.ctor === "Red")
      {
            return A5(RBNode_elm_builtin,
            Black,
            _p10._1,
            _p10._2,
            _p10._3,
            _p10._4);
         } else {
            return dict;
         }
   };
   var blackish = function (t) {
      var _p11 = t;
      if (_p11.ctor === "RBNode_elm_builtin") {
            var _p12 = _p11._0;
            return _U.eq(_p12,Black) || _U.eq(_p12,BBlack);
         } else {
            return true;
         }
   };
   var blacken = function (t) {
      var _p13 = t;
      if (_p13.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            return A5(RBNode_elm_builtin,
            Black,
            _p13._1,
            _p13._2,
            _p13._3,
            _p13._4);
         }
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      var _p14 = color;
      switch (_p14.ctor)
      {case "Black": return BBlack;
         case "Red": return Black;
         case "NBlack": return Red;
         default:
         return $Native$Debug.crash("Can\'t make a double black node more black!");}
   };
   var lessBlack = function (color) {
      var _p15 = color;
      switch (_p15.ctor)
      {case "BBlack": return Black;
         case "Black": return Red;
         case "Red": return NBlack;
         default:
         return $Native$Debug.crash("Can\'t make a negative black node less black!");}
   };
   var lessBlackTree = function (dict) {
      var _p16 = dict;
      if (_p16.ctor === "RBNode_elm_builtin") {
            return A5(RBNode_elm_builtin,
            lessBlack(_p16._0),
            _p16._1,
            _p16._2,
            _p16._3,
            _p16._4);
         } else {
            return RBEmpty_elm_builtin(LBlack);
         }
   };
   var balancedTree = function (col) {
      return function (xk) {
         return function (xv) {
            return function (yk) {
               return function (yv) {
                  return function (zk) {
                     return function (zv) {
                        return function (a) {
                           return function (b) {
                              return function (c) {
                                 return function (d) {
                                    return A5(RBNode_elm_builtin,
                                    lessBlack(col),
                                    yk,
                                    yv,
                                    A5(RBNode_elm_builtin,Black,xk,xv,a,b),
                                    A5(RBNode_elm_builtin,Black,zk,zv,c,d));
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var redden = function (t) {
      var _p17 = t;
      if (_p17.ctor === "RBEmpty_elm_builtin") {
            return $Native$Debug.crash("can\'t make a Leaf red");
         } else {
            return A5(RBNode_elm_builtin,
            Red,
            _p17._1,
            _p17._2,
            _p17._3,
            _p17._4);
         }
   };
   var balanceHelp = function (tree) {
      var _p18 = tree;
      _v31_6: do {
         _v31_5: do {
            _v31_4: do {
               _v31_3: do {
                  _v31_2: do {
                     _v31_1: do {
                        _v31_0: do {
                           if (_p18.ctor === "RBNode_elm_builtin") {
                                 if (_p18._3.ctor === "RBNode_elm_builtin") {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._3._0.ctor)
                                             {case "Red": switch (_p18._4._0.ctor)
                                                  {case "Red":
                                                     if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                       {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                                   {
                                                                         break _v31_2;
                                                                      } else {
                                                                         if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                                         {
                                                                               break _v31_3;
                                                                            } else {
                                                                               break _v31_6;
                                                                            }
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack":
                                                     if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                       {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_4;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     default:
                                                     if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                       {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_1;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }}
                                                case "NBlack": switch (_p18._4._0.ctor)
                                                  {case "Red":
                                                     if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                       {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_3;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack": if (_p18._0.ctor === "BBlack") {
                                                             if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                             {
                                                                   break _v31_4;
                                                                } else {
                                                                   if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default:
                                                     if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                       {
                                                             break _v31_5;
                                                          } else {
                                                             break _v31_6;
                                                          }}
                                                default: switch (_p18._4._0.ctor)
                                                  {case "Red":
                                                     if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                       {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_3;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }
                                                     case "NBlack":
                                                     if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                       {
                                                             break _v31_4;
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default: break _v31_6;}}
                                          } else {
                                             switch (_p18._3._0.ctor)
                                             {case "Red":
                                                if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                  {
                                                        break _v31_0;
                                                     } else {
                                                        if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                        {
                                                              break _v31_1;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                  {
                                                        break _v31_5;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          }
                                    } else {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._4._0.ctor)
                                             {case "Red":
                                                if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                  {
                                                        break _v31_2;
                                                     } else {
                                                        if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                        {
                                                              break _v31_3;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                  {
                                                        break _v31_4;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          } else {
                                             break _v31_6;
                                          }
                                    }
                              } else {
                                 break _v31_6;
                              }
                        } while (false);
                        return balancedTree(_p18._0)(_p18._3._3._1)(_p18._3._3._2)(_p18._3._1)(_p18._3._2)(_p18._1)(_p18._2)(_p18._3._3._3)(_p18._3._3._4)(_p18._3._4)(_p18._4);
                     } while (false);
                     return balancedTree(_p18._0)(_p18._3._1)(_p18._3._2)(_p18._3._4._1)(_p18._3._4._2)(_p18._1)(_p18._2)(_p18._3._3)(_p18._3._4._3)(_p18._3._4._4)(_p18._4);
                  } while (false);
                  return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._3._1)(_p18._4._3._2)(_p18._4._1)(_p18._4._2)(_p18._3)(_p18._4._3._3)(_p18._4._3._4)(_p18._4._4);
               } while (false);
               return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._1)(_p18._4._2)(_p18._4._4._1)(_p18._4._4._2)(_p18._3)(_p18._4._3)(_p18._4._4._3)(_p18._4._4._4);
            } while (false);
            return A5(RBNode_elm_builtin,
            Black,
            _p18._4._3._1,
            _p18._4._3._2,
            A5(RBNode_elm_builtin,
            Black,
            _p18._1,
            _p18._2,
            _p18._3,
            _p18._4._3._3),
            A5(balance,
            Black,
            _p18._4._1,
            _p18._4._2,
            _p18._4._3._4,
            redden(_p18._4._4)));
         } while (false);
         return A5(RBNode_elm_builtin,
         Black,
         _p18._3._4._1,
         _p18._3._4._2,
         A5(balance,
         Black,
         _p18._3._1,
         _p18._3._2,
         redden(_p18._3._3),
         _p18._3._4._3),
         A5(RBNode_elm_builtin,
         Black,
         _p18._1,
         _p18._2,
         _p18._3._4._4,
         _p18._4));
      } while (false);
      return tree;
   };
   var balance = F5(function (c,k,v,l,r) {
      var tree = A5(RBNode_elm_builtin,c,k,v,l,r);
      return blackish(tree) ? balanceHelp(tree) : tree;
   });
   var bubble = F5(function (c,k,v,l,r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode_elm_builtin,c,k,v,l,r);
   });
   var removeMax = F5(function (c,k,v,l,r) {
      var _p19 = r;
      if (_p19.ctor === "RBEmpty_elm_builtin") {
            return A3(rem,c,l,r);
         } else {
            return A5(bubble,
            c,
            k,
            v,
            l,
            A5(removeMax,_p19._0,_p19._1,_p19._2,_p19._3,_p19._4));
         }
   });
   var rem = F3(function (c,l,r) {
      var _p20 = {ctor: "_Tuple2",_0: l,_1: r};
      if (_p20._0.ctor === "RBEmpty_elm_builtin") {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p21 = c;
                  switch (_p21.ctor)
                  {case "Red": return RBEmpty_elm_builtin(LBlack);
                     case "Black": return RBEmpty_elm_builtin(LBBlack);
                     default:
                     return $Native$Debug.crash("cannot have bblack or nblack nodes at this point");}
               } else {
                  var _p24 = _p20._1._0;
                  var _p23 = _p20._0._0;
                  var _p22 = {ctor: "_Tuple3",_0: c,_1: _p23,_2: _p24};
                  if (_p22.ctor === "_Tuple3" && _p22._0.ctor === "Black" && _p22._1.ctor === "LBlack" && _p22._2.ctor === "Red")
                  {
                        return A5(RBNode_elm_builtin,
                        Black,
                        _p20._1._1,
                        _p20._1._2,
                        _p20._1._3,
                        _p20._1._4);
                     } else {
                        return A4(reportRemBug,
                        "Black/LBlack/Red",
                        c,
                        $Basics.toString(_p23),
                        $Basics.toString(_p24));
                     }
               }
         } else {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p27 = _p20._1._0;
                  var _p26 = _p20._0._0;
                  var _p25 = {ctor: "_Tuple3",_0: c,_1: _p26,_2: _p27};
                  if (_p25.ctor === "_Tuple3" && _p25._0.ctor === "Black" && _p25._1.ctor === "Red" && _p25._2.ctor === "LBlack")
                  {
                        return A5(RBNode_elm_builtin,
                        Black,
                        _p20._0._1,
                        _p20._0._2,
                        _p20._0._3,
                        _p20._0._4);
                     } else {
                        return A4(reportRemBug,
                        "Black/Red/LBlack",
                        c,
                        $Basics.toString(_p26),
                        $Basics.toString(_p27));
                     }
               } else {
                  var _p31 = _p20._0._2;
                  var _p30 = _p20._0._4;
                  var _p29 = _p20._0._1;
                  var l$ = A5(removeMax,_p20._0._0,_p29,_p31,_p20._0._3,_p30);
                  var _p28 = A3(maxWithDefault,_p29,_p31,_p30);
                  var k = _p28._0;
                  var v = _p28._1;
                  return A5(bubble,c,k,v,l$,r);
               }
         }
   });
   var update = F3(function (k,alter,dict) {
      var up = function (dict) {
         var _p32 = dict;
         if (_p32.ctor === "RBEmpty_elm_builtin") {
               var _p33 = alter($Maybe.Nothing);
               if (_p33.ctor === "Nothing") {
                     return {ctor: "_Tuple2",_0: Same,_1: empty};
                  } else {
                     return {ctor: "_Tuple2"
                            ,_0: Insert
                            ,_1: A5(RBNode_elm_builtin,Red,k,_p33._0,empty,empty)};
                  }
            } else {
               var _p44 = _p32._2;
               var _p43 = _p32._4;
               var _p42 = _p32._3;
               var _p41 = _p32._1;
               var _p40 = _p32._0;
               var _p34 = A2($Basics.compare,k,_p41);
               switch (_p34.ctor)
               {case "EQ": var _p35 = alter($Maybe.Just(_p44));
                    if (_p35.ctor === "Nothing") {
                          return {ctor: "_Tuple2"
                                 ,_0: Remove
                                 ,_1: A3(rem,_p40,_p42,_p43)};
                       } else {
                          return {ctor: "_Tuple2"
                                 ,_0: Same
                                 ,_1: A5(RBNode_elm_builtin,_p40,_p41,_p35._0,_p42,_p43)};
                       }
                  case "LT": var _p36 = up(_p42);
                    var flag = _p36._0;
                    var newLeft = _p36._1;
                    var _p37 = flag;
                    switch (_p37.ctor)
                    {case "Same": return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,newLeft,_p43)};
                       case "Insert": return {ctor: "_Tuple2"
                                             ,_0: Insert
                                             ,_1: A5(balance,_p40,_p41,_p44,newLeft,_p43)};
                       default: return {ctor: "_Tuple2"
                                       ,_0: Remove
                                       ,_1: A5(bubble,_p40,_p41,_p44,newLeft,_p43)};}
                  default: var _p38 = up(_p43);
                    var flag = _p38._0;
                    var newRight = _p38._1;
                    var _p39 = flag;
                    switch (_p39.ctor)
                    {case "Same": return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,_p42,newRight)};
                       case "Insert": return {ctor: "_Tuple2"
                                             ,_0: Insert
                                             ,_1: A5(balance,_p40,_p41,_p44,_p42,newRight)};
                       default: return {ctor: "_Tuple2"
                                       ,_0: Remove
                                       ,_1: A5(bubble,_p40,_p41,_p44,_p42,newRight)};}}
            }
      };
      var _p45 = up(dict);
      var flag = _p45._0;
      var updatedDict = _p45._1;
      var _p46 = flag;
      switch (_p46.ctor)
      {case "Same": return updatedDict;
         case "Insert": return ensureBlackRoot(updatedDict);
         default: return blacken(updatedDict);}
   });
   var insert = F3(function (key,value,dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,value) {
      return A3(insert,key,value,empty);
   });
   var union = F2(function (t1,t2) {
      return A3(foldl,insert,t2,t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_p47,dict) {
         var _p48 = _p47;
         return A3(insert,_p48._0,_p48._1,dict);
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,dictionary) {
      var add = F3(function (key,value,dict) {
         return A2(predicate,key,value) ? A3(insert,
         key,
         value,
         dict) : dict;
      });
      return A3(foldl,add,empty,dictionary);
   });
   var intersect = F2(function (t1,t2) {
      return A2(filter,
      F2(function (k,_p49) {    return A2(member,k,t2);}),
      t1);
   });
   var partition = F2(function (predicate,dict) {
      var add = F3(function (key,value,_p50) {
         var _p51 = _p50;
         var _p53 = _p51._1;
         var _p52 = _p51._0;
         return A2(predicate,key,value) ? {ctor: "_Tuple2"
                                          ,_0: A3(insert,key,value,_p52)
                                          ,_1: _p53} : {ctor: "_Tuple2"
                                                       ,_0: _p52
                                                       ,_1: A3(insert,key,value,_p53)};
      });
      return A3(foldl,add,{ctor: "_Tuple2",_0: empty,_1: empty},dict);
   });
   var remove = F2(function (key,dict) {
      return A3(update,key,$Basics.always($Maybe.Nothing),dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {    return A2(remove,k,t);}),
      t1,
      t2);
   });
   return _elm.Dict.values = {_op: _op
                             ,empty: empty
                             ,singleton: singleton
                             ,insert: insert
                             ,update: update
                             ,isEmpty: isEmpty
                             ,get: get
                             ,remove: remove
                             ,member: member
                             ,size: size
                             ,filter: filter
                             ,partition: partition
                             ,foldl: foldl
                             ,foldr: foldr
                             ,map: map
                             ,union: union
                             ,intersect: intersect
                             ,diff: diff
                             ,keys: keys
                             ,values: values
                             ,toList: toList
                             ,fromList: fromList};
};
Elm.Set = Elm.Set || {};
Elm.Set.make = function (_elm) {
   "use strict";
   _elm.Set = _elm.Set || {};
   if (_elm.Set.values) return _elm.Set.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm);
   var _op = {};
   var foldr = F3(function (f,b,_p0) {
      var _p1 = _p0;
      return A3($Dict.foldr,
      F3(function (k,_p2,b) {    return A2(f,k,b);}),
      b,
      _p1._0);
   });
   var foldl = F3(function (f,b,_p3) {
      var _p4 = _p3;
      return A3($Dict.foldl,
      F3(function (k,_p5,b) {    return A2(f,k,b);}),
      b,
      _p4._0);
   });
   var toList = function (_p6) {
      var _p7 = _p6;
      return $Dict.keys(_p7._0);
   };
   var size = function (_p8) {
      var _p9 = _p8;
      return $Dict.size(_p9._0);
   };
   var member = F2(function (k,_p10) {
      var _p11 = _p10;
      return A2($Dict.member,k,_p11._0);
   });
   var isEmpty = function (_p12) {
      var _p13 = _p12;
      return $Dict.isEmpty(_p13._0);
   };
   var Set_elm_builtin = function (a) {
      return {ctor: "Set_elm_builtin",_0: a};
   };
   var empty = Set_elm_builtin($Dict.empty);
   var singleton = function (k) {
      return Set_elm_builtin(A2($Dict.singleton,
      k,
      {ctor: "_Tuple0"}));
   };
   var insert = F2(function (k,_p14) {
      var _p15 = _p14;
      return Set_elm_builtin(A3($Dict.insert,
      k,
      {ctor: "_Tuple0"},
      _p15._0));
   });
   var fromList = function (xs) {
      return A3($List.foldl,insert,empty,xs);
   };
   var map = F2(function (f,s) {
      return fromList(A2($List.map,f,toList(s)));
   });
   var remove = F2(function (k,_p16) {
      var _p17 = _p16;
      return Set_elm_builtin(A2($Dict.remove,k,_p17._0));
   });
   var union = F2(function (_p19,_p18) {
      var _p20 = _p19;
      var _p21 = _p18;
      return Set_elm_builtin(A2($Dict.union,_p20._0,_p21._0));
   });
   var intersect = F2(function (_p23,_p22) {
      var _p24 = _p23;
      var _p25 = _p22;
      return Set_elm_builtin(A2($Dict.intersect,_p24._0,_p25._0));
   });
   var diff = F2(function (_p27,_p26) {
      var _p28 = _p27;
      var _p29 = _p26;
      return Set_elm_builtin(A2($Dict.diff,_p28._0,_p29._0));
   });
   var filter = F2(function (p,_p30) {
      var _p31 = _p30;
      return Set_elm_builtin(A2($Dict.filter,
      F2(function (k,_p32) {    return p(k);}),
      _p31._0));
   });
   var partition = F2(function (p,_p33) {
      var _p34 = _p33;
      var _p35 = A2($Dict.partition,
      F2(function (k,_p36) {    return p(k);}),
      _p34._0);
      var p1 = _p35._0;
      var p2 = _p35._1;
      return {ctor: "_Tuple2"
             ,_0: Set_elm_builtin(p1)
             ,_1: Set_elm_builtin(p2)};
   });
   return _elm.Set.values = {_op: _op
                            ,empty: empty
                            ,singleton: singleton
                            ,insert: insert
                            ,remove: remove
                            ,isEmpty: isEmpty
                            ,member: member
                            ,size: size
                            ,foldl: foldl
                            ,foldr: foldr
                            ,map: map
                            ,filter: filter
                            ,partition: partition
                            ,union: union
                            ,intersect: intersect
                            ,diff: diff
                            ,toList: toList
                            ,fromList: fromList};
};
Elm.List = Elm.List || {};
Elm.List.Extra = Elm.List.Extra || {};
Elm.List.Extra.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   _elm.List.Extra = _elm.List.Extra || {};
   if (_elm.List.Extra.values) return _elm.List.Extra.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var zip5 = $List.map5(F5(function (v0,v1,v2,v3,v4) {
      return {ctor: "_Tuple5",_0: v0,_1: v1,_2: v2,_3: v3,_4: v4};
   }));
   var zip4 = $List.map4(F4(function (v0,v1,v2,v3) {
      return {ctor: "_Tuple4",_0: v0,_1: v1,_2: v2,_3: v3};
   }));
   var zip3 = $List.map3(F3(function (v0,v1,v2) {
      return {ctor: "_Tuple3",_0: v0,_1: v1,_2: v2};
   }));
   var zip = $List.map2(F2(function (v0,v1) {
      return {ctor: "_Tuple2",_0: v0,_1: v1};
   }));
   var isPrefixOf = function (prefix) {
      return function (_p0) {
         return A2($List.all,
         $Basics.identity,
         A3($List.map2,
         F2(function (x,y) {    return _U.eq(x,y);}),
         prefix,
         _p0));
      };
   };
   var isSuffixOf = F2(function (suffix,xs) {
      return A2(isPrefixOf,
      $List.reverse(suffix),
      $List.reverse(xs));
   });
   var selectSplit = function (xs) {
      var _p1 = xs;
      if (_p1.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p5 = _p1._1;
            var _p4 = _p1._0;
            return A2($List._op["::"],
            {ctor: "_Tuple3",_0: _U.list([]),_1: _p4,_2: _p5},
            A2($List.map,
            function (_p2) {
               var _p3 = _p2;
               return {ctor: "_Tuple3"
                      ,_0: A2($List._op["::"],_p4,_p3._0)
                      ,_1: _p3._1
                      ,_2: _p3._2};
            },
            selectSplit(_p5)));
         }
   };
   var select = function (xs) {
      var _p6 = xs;
      if (_p6.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p10 = _p6._1;
            var _p9 = _p6._0;
            return A2($List._op["::"],
            {ctor: "_Tuple2",_0: _p9,_1: _p10},
            A2($List.map,
            function (_p7) {
               var _p8 = _p7;
               return {ctor: "_Tuple2"
                      ,_0: _p8._0
                      ,_1: A2($List._op["::"],_p9,_p8._1)};
            },
            select(_p10)));
         }
   };
   var tailsHelp = F2(function (e,list) {
      var _p11 = list;
      if (_p11.ctor === "::") {
            var _p12 = _p11._0;
            return A2($List._op["::"],
            A2($List._op["::"],e,_p12),
            A2($List._op["::"],_p12,_p11._1));
         } else {
            return _U.list([]);
         }
   });
   var tails = A2($List.foldr,tailsHelp,_U.list([_U.list([])]));
   var isInfixOf = F2(function (infix,xs) {
      return A2($List.any,isPrefixOf(infix),tails(xs));
   });
   var inits = A2($List.foldr,
   F2(function (e,acc) {
      return A2($List._op["::"],
      _U.list([]),
      A2($List.map,
      F2(function (x,y) {    return A2($List._op["::"],x,y);})(e),
      acc));
   }),
   _U.list([_U.list([])]));
   var groupByTransitive = F2(function (cmp,xs$) {
      var _p13 = xs$;
      if (_p13.ctor === "[]") {
            return _U.list([]);
         } else {
            if (_p13._1.ctor === "[]") {
                  return _U.list([_U.list([_p13._0])]);
               } else {
                  var _p15 = _p13._0;
                  var _p14 = A2(groupByTransitive,cmp,_p13._1);
                  if (_p14.ctor === "::") {
                        return A2(cmp,_p15,_p13._1._0) ? A2($List._op["::"],
                        A2($List._op["::"],_p15,_p14._0),
                        _p14._1) : A2($List._op["::"],_U.list([_p15]),_p14);
                     } else {
                        return _U.list([]);
                     }
               }
         }
   });
   var stripPrefix = F2(function (prefix,xs) {
      var step = F2(function (e,m) {
         var _p16 = m;
         if (_p16.ctor === "Nothing") {
               return $Maybe.Nothing;
            } else {
               if (_p16._0.ctor === "[]") {
                     return $Maybe.Nothing;
                  } else {
                     return _U.eq(e,
                     _p16._0._0) ? $Maybe.Just(_p16._0._1) : $Maybe.Nothing;
                  }
            }
      });
      return A3($List.foldl,step,$Maybe.Just(xs),prefix);
   });
   var dropWhileEnd = function (p) {
      return A2($List.foldr,
      F2(function (x,xs) {
         return p(x) && $List.isEmpty(xs) ? _U.list([]) : A2($List._op["::"],
         x,
         xs);
      }),
      _U.list([]));
   };
   var takeWhileEnd = function (p) {
      var step = F2(function (x,_p17) {
         var _p18 = _p17;
         var _p19 = _p18._0;
         return p(x) && _p18._1 ? {ctor: "_Tuple2"
                                  ,_0: A2($List._op["::"],x,_p19)
                                  ,_1: true} : {ctor: "_Tuple2",_0: _p19,_1: false};
      });
      return function (_p20) {
         return $Basics.fst(A3($List.foldr,
         step,
         {ctor: "_Tuple2",_0: _U.list([]),_1: true},
         _p20));
      };
   };
   var splitAt = F2(function (n,xs) {
      return {ctor: "_Tuple2"
             ,_0: A2($List.take,n,xs)
             ,_1: A2($List.drop,n,xs)};
   });
   var unfoldr = F2(function (f,seed) {
      var _p21 = f(seed);
      if (_p21.ctor === "Nothing") {
            return _U.list([]);
         } else {
            return A2($List._op["::"],
            _p21._0._0,
            A2(unfoldr,f,_p21._0._1));
         }
   });
   var scanr1 = F2(function (f,xs$) {
      var _p22 = xs$;
      if (_p22.ctor === "[]") {
            return _U.list([]);
         } else {
            if (_p22._1.ctor === "[]") {
                  return _U.list([_p22._0]);
               } else {
                  var _p23 = A2(scanr1,f,_p22._1);
                  if (_p23.ctor === "::") {
                        return A2($List._op["::"],A2(f,_p22._0,_p23._0),_p23);
                     } else {
                        return _U.list([]);
                     }
               }
         }
   });
   var scanr = F3(function (f,acc,xs$) {
      var _p24 = xs$;
      if (_p24.ctor === "[]") {
            return _U.list([acc]);
         } else {
            var _p25 = A3(scanr,f,acc,_p24._1);
            if (_p25.ctor === "::") {
                  return A2($List._op["::"],A2(f,_p24._0,_p25._0),_p25);
               } else {
                  return _U.list([]);
               }
         }
   });
   var scanl1 = F2(function (f,xs$) {
      var _p26 = xs$;
      if (_p26.ctor === "[]") {
            return _U.list([]);
         } else {
            return A3($List.scanl,f,_p26._0,_p26._1);
         }
   });
   var foldr1 = F2(function (f,xs) {
      var mf = F2(function (x,m) {
         return $Maybe.Just(function () {
            var _p27 = m;
            if (_p27.ctor === "Nothing") {
                  return x;
               } else {
                  return A2(f,x,_p27._0);
               }
         }());
      });
      return A3($List.foldr,mf,$Maybe.Nothing,xs);
   });
   var foldl1 = F2(function (f,xs) {
      var mf = F2(function (x,m) {
         return $Maybe.Just(function () {
            var _p28 = m;
            if (_p28.ctor === "Nothing") {
                  return x;
               } else {
                  return A2(f,_p28._0,x);
               }
         }());
      });
      return A3($List.foldl,mf,$Maybe.Nothing,xs);
   });
   var interweaveHelp = F3(function (l1,l2,acc) {
      interweaveHelp: while (true) {
         var _p29 = {ctor: "_Tuple2",_0: l1,_1: l2};
         _v17_1: do {
            if (_p29._0.ctor === "::") {
                  if (_p29._1.ctor === "::") {
                        var _v18 = _p29._0._1,
                        _v19 = _p29._1._1,
                        _v20 = A2($Basics._op["++"],
                        acc,
                        _U.list([_p29._0._0,_p29._1._0]));
                        l1 = _v18;
                        l2 = _v19;
                        acc = _v20;
                        continue interweaveHelp;
                     } else {
                        break _v17_1;
                     }
               } else {
                  if (_p29._1.ctor === "[]") {
                        break _v17_1;
                     } else {
                        return A2($Basics._op["++"],acc,_p29._1);
                     }
               }
         } while (false);
         return A2($Basics._op["++"],acc,_p29._0);
      }
   });
   var interweave = F2(function (l1,l2) {
      return A3(interweaveHelp,l1,l2,_U.list([]));
   });
   var permutations = function (xs$) {
      var _p30 = xs$;
      if (_p30.ctor === "[]") {
            return _U.list([_U.list([])]);
         } else {
            var f = function (_p31) {
               var _p32 = _p31;
               return A2($List.map,
               F2(function (x,y) {
                  return A2($List._op["::"],x,y);
               })(_p32._0),
               permutations(_p32._1));
            };
            return A2($List.concatMap,f,select(_p30));
         }
   };
   var isPermutationOf = F2(function (permut,xs) {
      return A2($List.member,permut,permutations(xs));
   });
   var subsequencesNonEmpty = function (xs) {
      var _p33 = xs;
      if (_p33.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p34 = _p33._0;
            var f = F2(function (ys,r) {
               return A2($List._op["::"],
               ys,
               A2($List._op["::"],A2($List._op["::"],_p34,ys),r));
            });
            return A2($List._op["::"],
            _U.list([_p34]),
            A3($List.foldr,f,_U.list([]),subsequencesNonEmpty(_p33._1)));
         }
   };
   var subsequences = function (xs) {
      return A2($List._op["::"],
      _U.list([]),
      subsequencesNonEmpty(xs));
   };
   var isSubsequenceOf = F2(function (subseq,xs) {
      return A2($List.member,subseq,subsequences(xs));
   });
   var transpose = function (ll) {
      transpose: while (true) {
         var _p35 = ll;
         if (_p35.ctor === "[]") {
               return _U.list([]);
            } else {
               if (_p35._0.ctor === "[]") {
                     var _v25 = _p35._1;
                     ll = _v25;
                     continue transpose;
                  } else {
                     var _p36 = _p35._1;
                     var tails = A2($List.filterMap,$List.tail,_p36);
                     var heads = A2($List.filterMap,$List.head,_p36);
                     return A2($List._op["::"],
                     A2($List._op["::"],_p35._0._0,heads),
                     transpose(A2($List._op["::"],_p35._0._1,tails)));
                  }
            }
      }
   };
   var intercalate = function (xs) {
      return function (_p37) {
         return $List.concat(A2($List.intersperse,xs,_p37));
      };
   };
   var removeWhen = F2(function (pred,list) {
      return A2($List.filter,
      function (_p38) {
         return $Basics.not(pred(_p38));
      },
      list);
   });
   var singleton = function (x) {    return _U.list([x]);};
   var replaceIf = F3(function (predicate,replacement,list) {
      return A2($List.map,
      function (item) {
         return predicate(item) ? replacement : item;
      },
      list);
   });
   var findIndices = function (p) {
      return function (_p39) {
         return A2($List.map,
         $Basics.fst,
         A2($List.filter,
         function (_p40) {
            var _p41 = _p40;
            return p(_p41._1);
         },
         A2($List.indexedMap,
         F2(function (v0,v1) {
            return {ctor: "_Tuple2",_0: v0,_1: v1};
         }),
         _p39)));
      };
   };
   var findIndex = function (p) {
      return function (_p42) {
         return $List.head(A2(findIndices,p,_p42));
      };
   };
   var elemIndices = function (x) {
      return findIndices(F2(function (x,y) {
         return _U.eq(x,y);
      })(x));
   };
   var elemIndex = function (x) {
      return findIndex(F2(function (x,y) {
         return _U.eq(x,y);
      })(x));
   };
   var find = F2(function (predicate,list) {
      find: while (true) {
         var _p43 = list;
         if (_p43.ctor === "[]") {
               return $Maybe.Nothing;
            } else {
               var _p44 = _p43._0;
               if (predicate(_p44)) return $Maybe.Just(_p44); else {
                     var _v28 = predicate,_v29 = _p43._1;
                     predicate = _v28;
                     list = _v29;
                     continue find;
                  }
            }
      }
   });
   var notMember = function (x) {
      return function (_p45) {
         return $Basics.not(A2($List.member,x,_p45));
      };
   };
   var andThen = $Basics.flip($List.concatMap);
   var lift2 = F3(function (f,la,lb) {
      return A2(andThen,
      la,
      function (a) {
         return A2(andThen,
         lb,
         function (b) {
            return _U.list([A2(f,a,b)]);
         });
      });
   });
   var lift3 = F4(function (f,la,lb,lc) {
      return A2(andThen,
      la,
      function (a) {
         return A2(andThen,
         lb,
         function (b) {
            return A2(andThen,
            lc,
            function (c) {
               return _U.list([A3(f,a,b,c)]);
            });
         });
      });
   });
   var lift4 = F5(function (f,la,lb,lc,ld) {
      return A2(andThen,
      la,
      function (a) {
         return A2(andThen,
         lb,
         function (b) {
            return A2(andThen,
            lc,
            function (c) {
               return A2(andThen,
               ld,
               function (d) {
                  return _U.list([A4(f,a,b,c,d)]);
               });
            });
         });
      });
   });
   var andMap = F2(function (fl,l) {
      return A3($List.map2,
      F2(function (x,y) {    return x(y);}),
      fl,
      l);
   });
   var dropDuplicates = function (list) {
      var step = F2(function (next,_p46) {
         var _p47 = _p46;
         var _p49 = _p47._0;
         var _p48 = _p47._1;
         return A2($Set.member,next,_p49) ? {ctor: "_Tuple2"
                                            ,_0: _p49
                                            ,_1: _p48} : {ctor: "_Tuple2"
                                                         ,_0: A2($Set.insert,next,_p49)
                                                         ,_1: A2($List._op["::"],next,_p48)};
      });
      return $List.reverse($Basics.snd(A3($List.foldl,
      step,
      {ctor: "_Tuple2",_0: $Set.empty,_1: _U.list([])},
      list)));
   };
   var dropWhile = F2(function (predicate,list) {
      dropWhile: while (true) {
         var _p50 = list;
         if (_p50.ctor === "[]") {
               return _U.list([]);
            } else {
               if (predicate(_p50._0)) {
                     var _v32 = predicate,_v33 = _p50._1;
                     predicate = _v32;
                     list = _v33;
                     continue dropWhile;
                  } else return list;
            }
      }
   });
   var takeWhile = F2(function (predicate,list) {
      var _p51 = list;
      if (_p51.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p52 = _p51._0;
            return predicate(_p52) ? A2($List._op["::"],
            _p52,
            A2(takeWhile,predicate,_p51._1)) : _U.list([]);
         }
   });
   var span = F2(function (p,xs) {
      return {ctor: "_Tuple2"
             ,_0: A2(takeWhile,p,xs)
             ,_1: A2(dropWhile,p,xs)};
   });
   var $break = function (p) {
      return span(function (_p53) {
         return $Basics.not(p(_p53));
      });
   };
   var groupBy = F2(function (eq,xs$) {
      var _p54 = xs$;
      if (_p54.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p56 = _p54._0;
            var _p55 = A2(span,eq(_p56),_p54._1);
            var ys = _p55._0;
            var zs = _p55._1;
            return A2($List._op["::"],
            A2($List._op["::"],_p56,ys),
            A2(groupBy,eq,zs));
         }
   });
   var group = groupBy(F2(function (x,y) {
      return _U.eq(x,y);
   }));
   var minimumBy = F2(function (f,ls) {
      var minBy = F2(function (x,_p57) {
         var _p58 = _p57;
         var _p59 = _p58._1;
         var fx = f(x);
         return _U.cmp(fx,_p59) < 0 ? {ctor: "_Tuple2"
                                      ,_0: x
                                      ,_1: fx} : {ctor: "_Tuple2",_0: _p58._0,_1: _p59};
      });
      var _p60 = ls;
      if (_p60.ctor === "::") {
            if (_p60._1.ctor === "[]") {
                  return $Maybe.Just(_p60._0);
               } else {
                  var _p61 = _p60._0;
                  return $Maybe.Just($Basics.fst(A3($List.foldl,
                  minBy,
                  {ctor: "_Tuple2",_0: _p61,_1: f(_p61)},
                  _p60._1)));
               }
         } else {
            return $Maybe.Nothing;
         }
   });
   var maximumBy = F2(function (f,ls) {
      var maxBy = F2(function (x,_p62) {
         var _p63 = _p62;
         var _p64 = _p63._1;
         var fx = f(x);
         return _U.cmp(fx,_p64) > 0 ? {ctor: "_Tuple2"
                                      ,_0: x
                                      ,_1: fx} : {ctor: "_Tuple2",_0: _p63._0,_1: _p64};
      });
      var _p65 = ls;
      if (_p65.ctor === "::") {
            if (_p65._1.ctor === "[]") {
                  return $Maybe.Just(_p65._0);
               } else {
                  var _p66 = _p65._0;
                  return $Maybe.Just($Basics.fst(A3($List.foldl,
                  maxBy,
                  {ctor: "_Tuple2",_0: _p66,_1: f(_p66)},
                  _p65._1)));
               }
         } else {
            return $Maybe.Nothing;
         }
   });
   var uncons = function (xs) {
      var _p67 = xs;
      if (_p67.ctor === "[]") {
            return $Maybe.Nothing;
         } else {
            return $Maybe.Just({ctor: "_Tuple2"
                               ,_0: _p67._0
                               ,_1: _p67._1});
         }
   };
   var iterate = F2(function (f,x) {
      var _p68 = f(x);
      if (_p68.ctor === "Just") {
            return A2($List._op["::"],x,A2(iterate,f,_p68._0));
         } else {
            return _U.list([x]);
         }
   });
   var getAt = F2(function (xs,idx) {
      return $List.head(A2($List.drop,idx,xs));
   });
   _op["!!"] = getAt;
   var init = function () {
      var maybe = F2(function (d,f) {
         return function (_p69) {
            return A2($Maybe.withDefault,d,A2($Maybe.map,f,_p69));
         };
      });
      return A2($List.foldr,
      function (_p70) {
         return A2(F2(function (x,y) {
            return function (_p71) {
               return x(y(_p71));
            };
         }),
         $Maybe.Just,
         A2(maybe,
         _U.list([]),
         F2(function (x,y) {
            return A2($List._op["::"],x,y);
         })(_p70)));
      },
      $Maybe.Nothing);
   }();
   var last = foldl1($Basics.flip($Basics.always));
   return _elm.List.Extra.values = {_op: _op
                                   ,last: last
                                   ,init: init
                                   ,getAt: getAt
                                   ,uncons: uncons
                                   ,minimumBy: minimumBy
                                   ,maximumBy: maximumBy
                                   ,andMap: andMap
                                   ,andThen: andThen
                                   ,takeWhile: takeWhile
                                   ,dropWhile: dropWhile
                                   ,dropDuplicates: dropDuplicates
                                   ,replaceIf: replaceIf
                                   ,singleton: singleton
                                   ,removeWhen: removeWhen
                                   ,iterate: iterate
                                   ,intercalate: intercalate
                                   ,transpose: transpose
                                   ,subsequences: subsequences
                                   ,permutations: permutations
                                   ,interweave: interweave
                                   ,foldl1: foldl1
                                   ,foldr1: foldr1
                                   ,scanl1: scanl1
                                   ,scanr: scanr
                                   ,scanr1: scanr1
                                   ,unfoldr: unfoldr
                                   ,splitAt: splitAt
                                   ,takeWhileEnd: takeWhileEnd
                                   ,dropWhileEnd: dropWhileEnd
                                   ,span: span
                                   ,$break: $break
                                   ,stripPrefix: stripPrefix
                                   ,group: group
                                   ,groupBy: groupBy
                                   ,groupByTransitive: groupByTransitive
                                   ,inits: inits
                                   ,tails: tails
                                   ,select: select
                                   ,selectSplit: selectSplit
                                   ,isPrefixOf: isPrefixOf
                                   ,isSuffixOf: isSuffixOf
                                   ,isInfixOf: isInfixOf
                                   ,isSubsequenceOf: isSubsequenceOf
                                   ,isPermutationOf: isPermutationOf
                                   ,notMember: notMember
                                   ,find: find
                                   ,elemIndex: elemIndex
                                   ,elemIndices: elemIndices
                                   ,findIndex: findIndex
                                   ,findIndices: findIndices
                                   ,zip: zip
                                   ,zip3: zip3
                                   ,zip4: zip4
                                   ,zip5: zip5
                                   ,lift2: lift2
                                   ,lift3: lift3
                                   ,lift4: lift4};
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: '_Array',
		height: 0,
		table: []
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				'Index ' + i + ' is out of range. Check the length of ' +
				'your array first or use getMaybe or getWithDefault.');
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height === 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len <= 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h === 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: '_Array',
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list === List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = [];
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i === M)
			{
				var leaf = {
					ctor: '_Array',
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table.splice(0, i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length === 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length === h)
		{
			var node = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length === M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height === 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: '_Array',
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed !== null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height === 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to === length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right === 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from === 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left === a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a, b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a, b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node);
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length;
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index === 0 || index === a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: '_Array',
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M === 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length);

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length === M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1, 0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height === 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: '_Array',
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h === tree.height)
		{
			return tree;
		}

		return {
			ctor: '_Array',
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: '_Array',
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height === 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t === 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length === 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray: toJSArray,
		fromJSArray: fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;
};

Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values) return _elm.Array.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var _op = {};
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var isEmpty = function (array) {
      return _U.eq(length(array),0);
   };
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,array) {
      return _U.cmp(0,i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,arr) {
      var update = F2(function (x,xs) {
         return isOkay(x) ? A2($Native$Array.push,x,xs) : xs;
      });
      return A3($Native$Array.foldl,update,$Native$Array.empty,arr);
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2",_0: v0,_1: v1};
      }),
      _U.range(0,$Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,n,$Basics.always(e));
   });
   var Array = {ctor: "Array"};
   return _elm.Array.values = {_op: _op
                              ,empty: empty
                              ,repeat: repeat
                              ,initialize: initialize
                              ,fromList: fromList
                              ,isEmpty: isEmpty
                              ,length: length
                              ,push: push
                              ,append: append
                              ,get: get
                              ,set: set
                              ,slice: slice
                              ,toList: toList
                              ,toIndexedList: toIndexedList
                              ,map: map
                              ,indexedMap: indexedMap
                              ,filter: filter
                              ,foldl: foldl
                              ,foldr: foldr};
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.Extra = Elm.Maybe.Extra || {};
Elm.Maybe.Extra.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   _elm.Maybe.Extra = _elm.Maybe.Extra || {};
   if (_elm.Maybe.Extra.values) return _elm.Maybe.Extra.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var traverseArray = function (f) {
      var step = F2(function (e,acc) {
         var _p0 = f(e);
         if (_p0.ctor === "Nothing") {
               return $Maybe.Nothing;
            } else {
               return A2($Maybe.map,$Array.push(_p0._0),acc);
            }
      });
      return A2($Array.foldl,step,$Maybe.Just($Array.empty));
   };
   var combineArray = traverseArray($Basics.identity);
   var traverse = function (f) {
      var step = F2(function (e,acc) {
         var _p1 = f(e);
         if (_p1.ctor === "Nothing") {
               return $Maybe.Nothing;
            } else {
               return A2($Maybe.map,
               F2(function (x,y) {
                  return A2($List._op["::"],x,y);
               })(_p1._0),
               acc);
            }
      });
      return A2($List.foldr,step,$Maybe.Just(_U.list([])));
   };
   var combine = traverse($Basics.identity);
   var maybeToArray = function (m) {
      var _p2 = m;
      if (_p2.ctor === "Nothing") {
            return $Array.empty;
         } else {
            return A2($Array.repeat,1,_p2._0);
         }
   };
   var maybeToList = function (m) {
      var _p3 = m;
      if (_p3.ctor === "Nothing") {
            return _U.list([]);
         } else {
            return _U.list([_p3._0]);
         }
   };
   var or = F2(function (ma,mb) {
      var _p4 = ma;
      if (_p4.ctor === "Nothing") {
            return mb;
         } else {
            return ma;
         }
   });
   var andMap = F2(function (f,x) {
      return A2($Maybe.andThen,
      x,
      function (x$) {
         return A2($Maybe.andThen,
         f,
         function (f$) {
            return $Maybe.Just(f$(x$));
         });
      });
   });
   var map5 = F6(function (f,a,b,c,d,e) {
      return A2(andMap,
      A2(andMap,A2(andMap,A2(andMap,A2($Maybe.map,f,a),b),c),d),
      e);
   });
   var map4 = F5(function (f,a,b,c,d) {
      return A2(andMap,
      A2(andMap,A2(andMap,A2($Maybe.map,f,a),b),c),
      d);
   });
   var map3 = F4(function (f,a,b,c) {
      return A2(andMap,A2(andMap,A2($Maybe.map,f,a),b),c);
   });
   var map2 = F3(function (f,a,b) {
      return A2(andMap,A2($Maybe.map,f,a),b);
   });
   var next = map2($Basics.flip($Basics.always));
   var prev = map2($Basics.always);
   var mapDefault = F3(function (d,f,m) {
      return A2($Maybe.withDefault,d,A2($Maybe.map,f,m));
   });
   var isJust = function (m) {
      var _p5 = m;
      if (_p5.ctor === "Nothing") {
            return false;
         } else {
            return true;
         }
   };
   var isNothing = function (m) {
      var _p6 = m;
      if (_p6.ctor === "Nothing") {
            return true;
         } else {
            return false;
         }
   };
   var join = function (mx) {
      var _p7 = mx;
      if (_p7.ctor === "Just") {
            return _p7._0;
         } else {
            return $Maybe.Nothing;
         }
   };
   _op["?"] = F2(function (mx,x) {
      return A2($Maybe.withDefault,x,mx);
   });
   return _elm.Maybe.Extra.values = {_op: _op
                                    ,join: join
                                    ,isNothing: isNothing
                                    ,isJust: isJust
                                    ,map2: map2
                                    ,map3: map3
                                    ,map4: map4
                                    ,map5: map5
                                    ,mapDefault: mapDefault
                                    ,andMap: andMap
                                    ,next: next
                                    ,prev: prev
                                    ,or: or
                                    ,maybeToList: maybeToList
                                    ,maybeToArray: maybeToArray
                                    ,traverse: traverse
                                    ,combine: combine
                                    ,traverseArray: traverseArray
                                    ,combineArray: combineArray};
};
Elm.Native.Time = {};

Elm.Native.Time.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input, state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value, initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2(NS.map, fst, NS.timestamp(ticker));
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new Date(t); },
		read: read
	};
};

Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values) return _elm.Time.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var delay = $Native$Signal.delay;
   var since = F2(function (time,signal) {
      var stop = A2($Signal.map,
      $Basics.always(-1),
      A2(delay,time,signal));
      var start = A2($Signal.map,$Basics.always(1),signal);
      var delaydiff = A3($Signal.foldp,
      F2(function (x,y) {    return x + y;}),
      0,
      A2($Signal.merge,start,stop));
      return A2($Signal.map,
      F2(function (x,y) {    return !_U.eq(x,y);})(0),
      delaydiff);
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {
      return A2(fpsWhen,targetFrames,$Signal.constant(true));
   };
   var inMilliseconds = function (t) {    return t;};
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {    return t / hour;};
   var inMinutes = function (t) {    return t / minute;};
   var inSeconds = function (t) {    return t / second;};
   return _elm.Time.values = {_op: _op
                             ,millisecond: millisecond
                             ,second: second
                             ,minute: minute
                             ,hour: hour
                             ,inMilliseconds: inMilliseconds
                             ,inSeconds: inSeconds
                             ,inMinutes: inMinutes
                             ,inHours: inHours
                             ,fps: fps
                             ,fpsWhen: fpsWhen
                             ,every: every
                             ,timestamp: timestamp
                             ,delay: delay
                             ,since: since};
};
Elm.Native.Json = {};

Elm.Native.Json.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value !== 'number') {
			crash('an Int', value);
		}

		if (value < 2147483647 && value > -2147483647 && (value | 0) === value) {
			return value;
		}

		if (isFinite(value) && !(value % 1)) {
			return value;
		}

		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i--; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i--; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value)
				{
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash('an object', value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		};
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		};
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		};
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		};
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		};
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};
};

Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values) return _elm.Json.Encode.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var _op = {};
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   return _elm.Json.Encode.values = {_op: _op
                                    ,encode: encode
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,object: object};
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values) return _elm.Json.Decode.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,decoder) {
      return A3($List.foldr,
      F2(function (x,y) {    return A2(_op[":="],x,y);}),
      decoder,
      fields);
   });
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {
      return A2(map,$Dict.fromList,keyValuePairs(decoder));
   };
   var Decoder = {ctor: "Decoder"};
   return _elm.Json.Decode.values = {_op: _op
                                    ,decodeString: decodeString
                                    ,decodeValue: decodeValue
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,tuple1: tuple1
                                    ,tuple2: tuple2
                                    ,tuple3: tuple3
                                    ,tuple4: tuple4
                                    ,tuple5: tuple5
                                    ,tuple6: tuple6
                                    ,tuple7: tuple7
                                    ,tuple8: tuple8
                                    ,at: at
                                    ,object1: object1
                                    ,object2: object2
                                    ,object3: object3
                                    ,object4: object4
                                    ,object5: object5
                                    ,object6: object6
                                    ,object7: object7
                                    ,object8: object8
                                    ,keyValuePairs: keyValuePairs
                                    ,dict: dict
                                    ,maybe: maybe
                                    ,oneOf: oneOf
                                    ,map: map
                                    ,fail: fail
                                    ,succeed: succeed
                                    ,andThen: andThen
                                    ,value: value
                                    ,customDecoder: customDecoder};
};
Elm.Native.Effects = {};
Elm.Native.Effects.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Effects = localRuntime.Native.Effects || {};
	if (localRuntime.Native.Effects.values)
	{
		return localRuntime.Native.Effects.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);
	var Signal = Elm.Signal.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);


	// polyfill so things will work even if rAF is not available for some reason
	var _requestAnimationFrame =
		typeof requestAnimationFrame !== 'undefined'
			? requestAnimationFrame
			: function(cb) { setTimeout(cb, 1000 / 60); }
			;


	// batchedSending and sendCallback implement a small state machine in order
	// to schedule only one send(time) call per animation frame.
	//
	// Invariants:
	// 1. In the NO_REQUEST state, there is never a scheduled sendCallback.
	// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly
	//    one scheduled sendCallback.
	var NO_REQUEST = 0;
	var PENDING_REQUEST = 1;
	var EXTRA_REQUEST = 2;
	var state = NO_REQUEST;
	var messageArray = [];


	function batchedSending(address, tickMessages)
	{
		// insert ticks into the messageArray
		var foundAddress = false;

		for (var i = messageArray.length; i--; )
		{
			if (messageArray[i].address === address)
			{
				foundAddress = true;
				messageArray[i].tickMessages = A3(List.foldl, List.cons, messageArray[i].tickMessages, tickMessages);
				break;
			}
		}

		if (!foundAddress)
		{
			messageArray.push({ address: address, tickMessages: tickMessages });
		}

		// do the appropriate state transition
		switch (state)
		{
			case NO_REQUEST:
				_requestAnimationFrame(sendCallback);
				state = PENDING_REQUEST;
				break;
			case PENDING_REQUEST:
				state = PENDING_REQUEST;
				break;
			case EXTRA_REQUEST:
				state = PENDING_REQUEST;
				break;
		}
	}


	function sendCallback(time)
	{
		switch (state)
		{
			case NO_REQUEST:
				// This state should not be possible. How can there be no
				// request, yet somehow we are actively fulfilling a
				// request?
				throw new Error(
					'Unexpected send callback.\n' +
					'Please report this to <https://github.com/evancz/elm-effects/issues>.'
				);

			case PENDING_REQUEST:
				// At this point, we do not *know* that another frame is
				// needed, but we make an extra request to rAF just in
				// case. It's possible to drop a frame if rAF is called
				// too late, so we just do it preemptively.
				_requestAnimationFrame(sendCallback);
				state = EXTRA_REQUEST;

				// There's also stuff we definitely need to send.
				send(time);
				return;

			case EXTRA_REQUEST:
				// Turns out the extra request was not needed, so we will
				// stop calling rAF. No reason to call it all the time if
				// no one needs it.
				state = NO_REQUEST;
				return;
		}
	}


	function send(time)
	{
		for (var i = messageArray.length; i--; )
		{
			var messages = A3(
				List.foldl,
				F2( function(toAction, list) { return List.Cons(toAction(time), list); } ),
				List.Nil,
				messageArray[i].tickMessages
			);
			Task.perform( A2(Signal.send, messageArray[i].address, messages) );
		}
		messageArray = [];
	}


	function requestTickSending(address, tickMessages)
	{
		return Task.asyncFunction(function(callback) {
			batchedSending(address, tickMessages);
			callback(Task.succeed(Utils.Tuple0));
		});
	}


	return localRuntime.Native.Effects.values = {
		requestTickSending: F2(requestTickSending)
	};

};

Elm.Effects = Elm.Effects || {};
Elm.Effects.make = function (_elm) {
   "use strict";
   _elm.Effects = _elm.Effects || {};
   if (_elm.Effects.values) return _elm.Effects.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Effects = Elm.Native.Effects.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var ignore = function (task) {
      return A2($Task.map,$Basics.always({ctor: "_Tuple0"}),task);
   };
   var requestTickSending = $Native$Effects.requestTickSending;
   var toTaskHelp = F3(function (address,effect,_p0) {
      var _p1 = _p0;
      var _p5 = _p1._1;
      var _p4 = _p1;
      var _p3 = _p1._0;
      var _p2 = effect;
      switch (_p2.ctor)
      {case "Task": var reporter = A2($Task.andThen,
           _p2._0,
           function (answer) {
              return A2($Signal.send,address,_U.list([answer]));
           });
           return {ctor: "_Tuple2"
                  ,_0: A2($Task.andThen,
                  _p3,
                  $Basics.always(ignore($Task.spawn(reporter))))
                  ,_1: _p5};
         case "Tick": return {ctor: "_Tuple2"
                             ,_0: _p3
                             ,_1: A2($List._op["::"],_p2._0,_p5)};
         case "None": return _p4;
         default: return A3($List.foldl,toTaskHelp(address),_p4,_p2._0);}
   });
   var toTask = F2(function (address,effect) {
      var _p6 = A3(toTaskHelp,
      address,
      effect,
      {ctor: "_Tuple2"
      ,_0: $Task.succeed({ctor: "_Tuple0"})
      ,_1: _U.list([])});
      var combinedTask = _p6._0;
      var tickMessages = _p6._1;
      return $List.isEmpty(tickMessages) ? combinedTask : A2($Task.andThen,
      combinedTask,
      $Basics.always(A2(requestTickSending,address,tickMessages)));
   });
   var Never = function (a) {    return {ctor: "Never",_0: a};};
   var Batch = function (a) {    return {ctor: "Batch",_0: a};};
   var batch = Batch;
   var None = {ctor: "None"};
   var none = None;
   var Tick = function (a) {    return {ctor: "Tick",_0: a};};
   var tick = Tick;
   var Task = function (a) {    return {ctor: "Task",_0: a};};
   var task = Task;
   var map = F2(function (func,effect) {
      var _p7 = effect;
      switch (_p7.ctor)
      {case "Task": return Task(A2($Task.map,func,_p7._0));
         case "Tick": return Tick(function (_p8) {
              return func(_p7._0(_p8));
           });
         case "None": return None;
         default: return Batch(A2($List.map,map(func),_p7._0));}
   });
   return _elm.Effects.values = {_op: _op
                                ,none: none
                                ,task: task
                                ,tick: tick
                                ,map: map
                                ,batch: batch
                                ,toTask: toTask};
};
(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){

},{}],2:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":1}],3:[function(require,module,exports){
"use strict";

module.exports = function isObject(x) {
	return typeof x === "object" && x !== null;
};

},{}],4:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],5:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook.js")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook(propValue)) {
            removeProperty(node, propName, propValue, previous)
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined)
            }
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue)
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"../vnode/is-vhook.js":13,"is-object":3}],6:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vnode/is-vnode.js")
var isVText = require("../vnode/is-vtext.js")
var isWidget = require("../vnode/is-widget.js")
var handleThunk = require("../vnode/handle-thunk.js")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vnode/handle-thunk.js":11,"../vnode/is-vnode.js":14,"../vnode/is-vtext.js":15,"../vnode/is-widget.js":16,"./apply-properties":5,"global/document":2}],7:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],8:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vnode/is-widget.js")
var VPatch = require("../vnode/vpatch.js")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget)
    var newNode

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode
    } else {
        newNode = render(widget, renderOptions)
    }

    var parentNode = domNode.parentNode

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode)
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes
    var keyMap = {}
    var node
    var remove
    var insert

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i]
        node = childNodes[remove.from]
        if (remove.key) {
            keyMap[remove.key] = node
        }
        domNode.removeChild(node)
    }

    var length = childNodes.length
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j]
        node = keyMap[insert.key]
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"../vnode/is-widget.js":16,"../vnode/vpatch.js":19,"./apply-properties":5,"./create-element":6,"./update-widget":10}],9:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches) {
    return patchRecursive(rootNode, patches)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions) {
        renderOptions = { patch: patchRecursive }
        if (ownerDocument !== document) {
            renderOptions.document = ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":7,"./patch-op":8,"global/document":2,"x-is-array":4}],10:[function(require,module,exports){
var isWidget = require("../vnode/is-widget.js")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vnode/is-widget.js":16}],11:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":12,"./is-vnode":14,"./is-vtext":15,"./is-widget":16}],12:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],13:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

},{}],14:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":17}],15:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":17}],16:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],17:[function(require,module,exports){
module.exports = "2"

},{}],18:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var hasThunks = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        } else if (!hasThunks && isThunk(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hasThunks = hasThunks
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-thunk":12,"./is-vhook":13,"./is-vnode":14,"./is-widget":16,"./version":17}],19:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":17}],20:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":17}],21:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook")

module.exports = diffProps

function diffProps(a, b) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (aValue === bValue) {
            continue
        } else if (isObject(aValue) && isObject(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {}
                diff[aKey] = bValue
            } else if (isHook(bValue)) {
                 diff = diff || {}
                 diff[aKey] = bValue
            } else {
                var objectDiff = diffProps(aValue, bValue)
                if (objectDiff) {
                    diff = diff || {}
                    diff[aKey] = objectDiff
                }
            }
        } else {
            diff = diff || {}
            diff[aKey] = bValue
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

},{"../vnode/is-vhook":13,"is-object":3}],22:[function(require,module,exports){
var isArray = require("x-is-array")

var VPatch = require("../vnode/vpatch")
var isVNode = require("../vnode/is-vnode")
var isVText = require("../vnode/is-vtext")
var isWidget = require("../vnode/is-widget")
var isThunk = require("../vnode/is-thunk")
var handleThunk = require("../vnode/handle-thunk")

var diffProps = require("./diff-props")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index]
    var applyClear = false

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index)
            apply = patch[index]
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                applyClear = true
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            applyClear = true
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            applyClear = true
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
    }

    if (apply) {
        patch[index] = apply
    }

    if (applyClear) {
        clearState(a, patch, index)
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var orderedSet = reorder(aChildren, b.children)
    var bChildren = orderedSet.children

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ))
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index)
    destroyWidgets(vNode, patch, index)
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            )
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b)
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            )
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                unhook(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

function undefinedKeys(obj) {
    var result = {}

    for (var key in obj) {
        result[key] = undefined
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren)
    var bKeys = bChildIndex.keys
    var bFree = bChildIndex.free

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren)
    var aKeys = aChildIndex.keys
    var aFree = aChildIndex.free

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = []

    var freeIndex = 0
    var freeCount = bFree.length
    var deletedItems = 0

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i]
        var itemIndex

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key]
                newChildren.push(bChildren[itemIndex])

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++]
                newChildren.push(bChildren[itemIndex])
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex]

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j]

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem)
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem)
        }
    }

    var simulate = newChildren.slice()
    var simulateIndex = 0
    var removes = []
    var inserts = []
    var simulateItem

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k]
        simulateItem = simulate[simulateIndex]

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove(simulate, simulateIndex, null))
            simulateItem = simulate[simulateIndex]
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
                        simulateItem = simulate[simulateIndex]
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k})
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k})
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k})
                }
                k++
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove(simulate, simulateIndex, simulateItem.key))
            }
        }
        else {
            simulateIndex++
            k++
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex]
        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove(arr, index, key) {
    arr.splice(index, 1)

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {}
    var free = []
    var length = children.length

    for (var i = 0; i < length; i++) {
        var child = children[i]

        if (child.key) {
            keys[child.key] = i
        } else {
            free.push(i)
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free,     // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"../vnode/handle-thunk":11,"../vnode/is-thunk":12,"../vnode/is-vnode":14,"../vnode/is-vtext":15,"../vnode/is-widget":16,"../vnode/vpatch":19,"./diff-props":21,"x-is-array":4}],23:[function(require,module,exports){
var VNode = require('virtual-dom/vnode/vnode');
var VText = require('virtual-dom/vnode/vtext');
var diff = require('virtual-dom/vtree/diff');
var patch = require('virtual-dom/vdom/patch');
var createElement = require('virtual-dom/vdom/create-element');
var isHook = require("virtual-dom/vnode/is-vhook");


Elm.Native.VirtualDom = {};
Elm.Native.VirtualDom.make = function(elm)
{
	elm.Native = elm.Native || {};
	elm.Native.VirtualDom = elm.Native.VirtualDom || {};
	if (elm.Native.VirtualDom.values)
	{
		return elm.Native.VirtualDom.values;
	}

	var Element = Elm.Native.Graphics.Element.make(elm);
	var Json = Elm.Native.Json.make(elm);
	var List = Elm.Native.List.make(elm);
	var Signal = Elm.Native.Signal.make(elm);
	var Utils = Elm.Native.Utils.make(elm);

	var ATTRIBUTE_KEY = 'UniqueNameThatOthersAreVeryUnlikelyToUse';



	// VIRTUAL DOM NODES


	function text(string)
	{
		return new VText(string);
	}

	function node(name)
	{
		return F2(function(propertyList, contents) {
			return makeNode(name, propertyList, contents);
		});
	}


	// BUILD VIRTUAL DOME NODES


	function makeNode(name, propertyList, contents)
	{
		var props = listToProperties(propertyList);

		var key, namespace;
		// support keys
		if (props.key !== undefined)
		{
			key = props.key;
			props.key = undefined;
		}

		// support namespace
		if (props.namespace !== undefined)
		{
			namespace = props.namespace;
			props.namespace = undefined;
		}

		// ensure that setting text of an input does not move the cursor
		var useSoftSet =
			(name === 'input' || name === 'textarea')
			&& props.value !== undefined
			&& !isHook(props.value);

		if (useSoftSet)
		{
			props.value = SoftSetHook(props.value);
		}

		return new VNode(name, props, List.toArray(contents), key, namespace);
	}

	function listToProperties(list)
	{
		var object = {};
		while (list.ctor !== '[]')
		{
			var entry = list._0;
			if (entry.key === ATTRIBUTE_KEY)
			{
				object.attributes = object.attributes || {};
				object.attributes[entry.value.attrKey] = entry.value.attrValue;
			}
			else
			{
				object[entry.key] = entry.value;
			}
			list = list._1;
		}
		return object;
	}



	// PROPERTIES AND ATTRIBUTES


	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

	function attribute(key, value)
	{
		return {
			key: ATTRIBUTE_KEY,
			value: {
				attrKey: key,
				attrValue: value
			}
		};
	}



	// NAMESPACED ATTRIBUTES


	function attributeNS(namespace, key, value)
	{
		return {
			key: key,
			value: new AttributeHook(namespace, key, value)
		};
	}

	function AttributeHook(namespace, key, value)
	{
		if (!(this instanceof AttributeHook))
		{
			return new AttributeHook(namespace, key, value);
		}

		this.namespace = namespace;
		this.key = key;
		this.value = value;
	}

	AttributeHook.prototype.hook = function (node, prop, prev)
	{
		if (prev
			&& prev.type === 'AttributeHook'
			&& prev.value === this.value
			&& prev.namespace === this.namespace)
		{
			return;
		}

		node.setAttributeNS(this.namespace, prop, this.value);
	};

	AttributeHook.prototype.unhook = function (node, prop, next)
	{
		if (next
			&& next.type === 'AttributeHook'
			&& next.namespace === this.namespace)
		{
			return;
		}

		node.removeAttributeNS(this.namespace, this.key);
	};

	AttributeHook.prototype.type = 'AttributeHook';



	// EVENTS


	function on(name, options, decoder, createMessage)
	{
		function eventHandler(event)
		{
			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				if (options.stopPropagation)
				{
					event.stopPropagation();
				}
				if (options.preventDefault)
				{
					event.preventDefault();
				}
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property('on' + name, eventHandler);
	}

	function SoftSetHook(value)
	{
		if (!(this instanceof SoftSetHook))
		{
			return new SoftSetHook(value);
		}

		this.value = value;
	}

	SoftSetHook.prototype.hook = function (node, propertyName)
	{
		if (node[propertyName] !== this.value)
		{
			node[propertyName] = this.value;
		}
	};



	// INTEGRATION WITH ELEMENTS


	function ElementWidget(element)
	{
		this.element = element;
	}

	ElementWidget.prototype.type = "Widget";

	ElementWidget.prototype.init = function init()
	{
		return Element.render(this.element);
	};

	ElementWidget.prototype.update = function update(previous, node)
	{
		return Element.update(node, previous.element, this.element);
	};

	function fromElement(element)
	{
		return new ElementWidget(element);
	}

	function toElement(width, height, html)
	{
		return A3(Element.newElement, width, height, {
			ctor: 'Custom',
			type: 'evancz/elm-html',
			render: render,
			update: update,
			model: html
		});
	}



	// RENDER AND UPDATE


	function render(model)
	{
		var element = Element.createNode('div');
		element.appendChild(createElement(model));
		return element;
	}

	function update(node, oldModel, newModel)
	{
		updateAndReplace(node.firstChild, oldModel, newModel);
		return node;
	}

	function updateAndReplace(node, oldModel, newModel)
	{
		var patches = diff(oldModel, newModel);
		var newNode = patch(node, patches);
		return newNode;
	}



	// LAZINESS


	function lazyRef(fn, a)
	{
		function thunk()
		{
			return fn(a);
		}
		return new Thunk(fn, [a], thunk);
	}

	function lazyRef2(fn, a, b)
	{
		function thunk()
		{
			return A2(fn, a, b);
		}
		return new Thunk(fn, [a,b], thunk);
	}

	function lazyRef3(fn, a, b, c)
	{
		function thunk()
		{
			return A3(fn, a, b, c);
		}
		return new Thunk(fn, [a,b,c], thunk);
	}

	function Thunk(fn, args, thunk)
	{
		/* public (used by VirtualDom.js) */
		this.vnode = null;
		this.key = undefined;

		/* private */
		this.fn = fn;
		this.args = args;
		this.thunk = thunk;
	}

	Thunk.prototype.type = "Thunk";
	Thunk.prototype.render = renderThunk;

	function shouldUpdate(current, previous)
	{
		if (current.fn !== previous.fn)
		{
			return true;
		}

		// if it's the same function, we know the number of args must match
		var cargs = current.args;
		var pargs = previous.args;

		for (var i = cargs.length; i--; )
		{
			if (cargs[i] !== pargs[i])
			{
				return true;
			}
		}

		return false;
	}

	function renderThunk(previous)
	{
		if (previous == null || shouldUpdate(this, previous))
		{
			return this.thunk();
		}
		else
		{
			return previous.vnode;
		}
	}


	return elm.Native.VirtualDom.values = Elm.Native.VirtualDom.values = {
		node: node,
		text: text,
		on: F4(on),

		property: F2(property),
		attribute: F2(attribute),
		attributeNS: F3(attributeNS),

		lazy: F2(lazyRef),
		lazy2: F3(lazyRef2),
		lazy3: F4(lazyRef3),

		toElement: F3(toElement),
		fromElement: fromElement,

		render: createElement,
		updateAndReplace: updateAndReplace
	};
};

},{"virtual-dom/vdom/create-element":6,"virtual-dom/vdom/patch":9,"virtual-dom/vnode/is-vhook":13,"virtual-dom/vnode/vnode":18,"virtual-dom/vnode/vtext":20,"virtual-dom/vtree/diff":22}]},{},[23]);

Elm.VirtualDom = Elm.VirtualDom || {};
Elm.VirtualDom.make = function (_elm) {
   "use strict";
   _elm.VirtualDom = _elm.VirtualDom || {};
   if (_elm.VirtualDom.values) return _elm.VirtualDom.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$VirtualDom = Elm.Native.VirtualDom.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var lazy3 = $Native$VirtualDom.lazy3;
   var lazy2 = $Native$VirtualDom.lazy2;
   var lazy = $Native$VirtualDom.lazy;
   var defaultOptions = {stopPropagation: false
                        ,preventDefault: false};
   var Options = F2(function (a,b) {
      return {stopPropagation: a,preventDefault: b};
   });
   var onWithOptions = $Native$VirtualDom.on;
   var on = F3(function (eventName,decoder,toMessage) {
      return A4($Native$VirtualDom.on,
      eventName,
      defaultOptions,
      decoder,
      toMessage);
   });
   var attributeNS = $Native$VirtualDom.attributeNS;
   var attribute = $Native$VirtualDom.attribute;
   var property = $Native$VirtualDom.property;
   var Property = {ctor: "Property"};
   var fromElement = $Native$VirtualDom.fromElement;
   var toElement = $Native$VirtualDom.toElement;
   var text = $Native$VirtualDom.text;
   var node = $Native$VirtualDom.node;
   var Node = {ctor: "Node"};
   return _elm.VirtualDom.values = {_op: _op
                                   ,text: text
                                   ,node: node
                                   ,toElement: toElement
                                   ,fromElement: fromElement
                                   ,property: property
                                   ,attribute: attribute
                                   ,attributeNS: attributeNS
                                   ,on: on
                                   ,onWithOptions: onWithOptions
                                   ,defaultOptions: defaultOptions
                                   ,lazy: lazy
                                   ,lazy2: lazy2
                                   ,lazy3: lazy3
                                   ,Options: Options};
};
Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values) return _elm.Html.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var fromElement = $VirtualDom.fromElement;
   var toElement = $VirtualDom.toElement;
   var text = $VirtualDom.text;
   var node = $VirtualDom.node;
   var body = node("body");
   var section = node("section");
   var nav = node("nav");
   var article = node("article");
   var aside = node("aside");
   var h1 = node("h1");
   var h2 = node("h2");
   var h3 = node("h3");
   var h4 = node("h4");
   var h5 = node("h5");
   var h6 = node("h6");
   var header = node("header");
   var footer = node("footer");
   var address = node("address");
   var main$ = node("main");
   var p = node("p");
   var hr = node("hr");
   var pre = node("pre");
   var blockquote = node("blockquote");
   var ol = node("ol");
   var ul = node("ul");
   var li = node("li");
   var dl = node("dl");
   var dt = node("dt");
   var dd = node("dd");
   var figure = node("figure");
   var figcaption = node("figcaption");
   var div = node("div");
   var a = node("a");
   var em = node("em");
   var strong = node("strong");
   var small = node("small");
   var s = node("s");
   var cite = node("cite");
   var q = node("q");
   var dfn = node("dfn");
   var abbr = node("abbr");
   var time = node("time");
   var code = node("code");
   var $var = node("var");
   var samp = node("samp");
   var kbd = node("kbd");
   var sub = node("sub");
   var sup = node("sup");
   var i = node("i");
   var b = node("b");
   var u = node("u");
   var mark = node("mark");
   var ruby = node("ruby");
   var rt = node("rt");
   var rp = node("rp");
   var bdi = node("bdi");
   var bdo = node("bdo");
   var span = node("span");
   var br = node("br");
   var wbr = node("wbr");
   var ins = node("ins");
   var del = node("del");
   var img = node("img");
   var iframe = node("iframe");
   var embed = node("embed");
   var object = node("object");
   var param = node("param");
   var video = node("video");
   var audio = node("audio");
   var source = node("source");
   var track = node("track");
   var canvas = node("canvas");
   var svg = node("svg");
   var math = node("math");
   var table = node("table");
   var caption = node("caption");
   var colgroup = node("colgroup");
   var col = node("col");
   var tbody = node("tbody");
   var thead = node("thead");
   var tfoot = node("tfoot");
   var tr = node("tr");
   var td = node("td");
   var th = node("th");
   var form = node("form");
   var fieldset = node("fieldset");
   var legend = node("legend");
   var label = node("label");
   var input = node("input");
   var button = node("button");
   var select = node("select");
   var datalist = node("datalist");
   var optgroup = node("optgroup");
   var option = node("option");
   var textarea = node("textarea");
   var keygen = node("keygen");
   var output = node("output");
   var progress = node("progress");
   var meter = node("meter");
   var details = node("details");
   var summary = node("summary");
   var menuitem = node("menuitem");
   var menu = node("menu");
   return _elm.Html.values = {_op: _op
                             ,node: node
                             ,text: text
                             ,toElement: toElement
                             ,fromElement: fromElement
                             ,body: body
                             ,section: section
                             ,nav: nav
                             ,article: article
                             ,aside: aside
                             ,h1: h1
                             ,h2: h2
                             ,h3: h3
                             ,h4: h4
                             ,h5: h5
                             ,h6: h6
                             ,header: header
                             ,footer: footer
                             ,address: address
                             ,main$: main$
                             ,p: p
                             ,hr: hr
                             ,pre: pre
                             ,blockquote: blockquote
                             ,ol: ol
                             ,ul: ul
                             ,li: li
                             ,dl: dl
                             ,dt: dt
                             ,dd: dd
                             ,figure: figure
                             ,figcaption: figcaption
                             ,div: div
                             ,a: a
                             ,em: em
                             ,strong: strong
                             ,small: small
                             ,s: s
                             ,cite: cite
                             ,q: q
                             ,dfn: dfn
                             ,abbr: abbr
                             ,time: time
                             ,code: code
                             ,$var: $var
                             ,samp: samp
                             ,kbd: kbd
                             ,sub: sub
                             ,sup: sup
                             ,i: i
                             ,b: b
                             ,u: u
                             ,mark: mark
                             ,ruby: ruby
                             ,rt: rt
                             ,rp: rp
                             ,bdi: bdi
                             ,bdo: bdo
                             ,span: span
                             ,br: br
                             ,wbr: wbr
                             ,ins: ins
                             ,del: del
                             ,img: img
                             ,iframe: iframe
                             ,embed: embed
                             ,object: object
                             ,param: param
                             ,video: video
                             ,audio: audio
                             ,source: source
                             ,track: track
                             ,canvas: canvas
                             ,svg: svg
                             ,math: math
                             ,table: table
                             ,caption: caption
                             ,colgroup: colgroup
                             ,col: col
                             ,tbody: tbody
                             ,thead: thead
                             ,tfoot: tfoot
                             ,tr: tr
                             ,td: td
                             ,th: th
                             ,form: form
                             ,fieldset: fieldset
                             ,legend: legend
                             ,label: label
                             ,input: input
                             ,button: button
                             ,select: select
                             ,datalist: datalist
                             ,optgroup: optgroup
                             ,option: option
                             ,textarea: textarea
                             ,keygen: keygen
                             ,output: output
                             ,progress: progress
                             ,meter: meter
                             ,details: details
                             ,summary: summary
                             ,menuitem: menuitem
                             ,menu: menu};
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values)
   return _elm.Html.Attributes.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var attribute = $VirtualDom.attribute;
   var contextmenu = function (value) {
      return A2(attribute,"contextmenu",value);
   };
   var property = $VirtualDom.property;
   var stringProperty = F2(function (name,string) {
      return A2(property,name,$Json$Encode.string(string));
   });
   var $class = function (name) {
      return A2(stringProperty,"className",name);
   };
   var id = function (name) {
      return A2(stringProperty,"id",name);
   };
   var title = function (name) {
      return A2(stringProperty,"title",name);
   };
   var accesskey = function ($char) {
      return A2(stringProperty,
      "accessKey",
      $String.fromChar($char));
   };
   var dir = function (value) {
      return A2(stringProperty,"dir",value);
   };
   var draggable = function (value) {
      return A2(stringProperty,"draggable",value);
   };
   var dropzone = function (value) {
      return A2(stringProperty,"dropzone",value);
   };
   var itemprop = function (value) {
      return A2(stringProperty,"itemprop",value);
   };
   var lang = function (value) {
      return A2(stringProperty,"lang",value);
   };
   var tabindex = function (n) {
      return A2(stringProperty,"tabIndex",$Basics.toString(n));
   };
   var charset = function (value) {
      return A2(stringProperty,"charset",value);
   };
   var content = function (value) {
      return A2(stringProperty,"content",value);
   };
   var httpEquiv = function (value) {
      return A2(stringProperty,"httpEquiv",value);
   };
   var language = function (value) {
      return A2(stringProperty,"language",value);
   };
   var src = function (value) {
      return A2(stringProperty,"src",value);
   };
   var height = function (value) {
      return A2(stringProperty,"height",$Basics.toString(value));
   };
   var width = function (value) {
      return A2(stringProperty,"width",$Basics.toString(value));
   };
   var alt = function (value) {
      return A2(stringProperty,"alt",value);
   };
   var preload = function (value) {
      return A2(stringProperty,"preload",value);
   };
   var poster = function (value) {
      return A2(stringProperty,"poster",value);
   };
   var kind = function (value) {
      return A2(stringProperty,"kind",value);
   };
   var srclang = function (value) {
      return A2(stringProperty,"srclang",value);
   };
   var sandbox = function (value) {
      return A2(stringProperty,"sandbox",value);
   };
   var srcdoc = function (value) {
      return A2(stringProperty,"srcdoc",value);
   };
   var type$ = function (value) {
      return A2(stringProperty,"type",value);
   };
   var value = function (value) {
      return A2(stringProperty,"value",value);
   };
   var placeholder = function (value) {
      return A2(stringProperty,"placeholder",value);
   };
   var accept = function (value) {
      return A2(stringProperty,"accept",value);
   };
   var acceptCharset = function (value) {
      return A2(stringProperty,"acceptCharset",value);
   };
   var action = function (value) {
      return A2(stringProperty,"action",value);
   };
   var autocomplete = function (bool) {
      return A2(stringProperty,"autocomplete",bool ? "on" : "off");
   };
   var autosave = function (value) {
      return A2(stringProperty,"autosave",value);
   };
   var enctype = function (value) {
      return A2(stringProperty,"enctype",value);
   };
   var formaction = function (value) {
      return A2(stringProperty,"formAction",value);
   };
   var list = function (value) {
      return A2(stringProperty,"list",value);
   };
   var minlength = function (n) {
      return A2(stringProperty,"minLength",$Basics.toString(n));
   };
   var maxlength = function (n) {
      return A2(stringProperty,"maxLength",$Basics.toString(n));
   };
   var method = function (value) {
      return A2(stringProperty,"method",value);
   };
   var name = function (value) {
      return A2(stringProperty,"name",value);
   };
   var pattern = function (value) {
      return A2(stringProperty,"pattern",value);
   };
   var size = function (n) {
      return A2(stringProperty,"size",$Basics.toString(n));
   };
   var $for = function (value) {
      return A2(stringProperty,"htmlFor",value);
   };
   var form = function (value) {
      return A2(stringProperty,"form",value);
   };
   var max = function (value) {
      return A2(stringProperty,"max",value);
   };
   var min = function (value) {
      return A2(stringProperty,"min",value);
   };
   var step = function (n) {
      return A2(stringProperty,"step",n);
   };
   var cols = function (n) {
      return A2(stringProperty,"cols",$Basics.toString(n));
   };
   var rows = function (n) {
      return A2(stringProperty,"rows",$Basics.toString(n));
   };
   var wrap = function (value) {
      return A2(stringProperty,"wrap",value);
   };
   var usemap = function (value) {
      return A2(stringProperty,"useMap",value);
   };
   var shape = function (value) {
      return A2(stringProperty,"shape",value);
   };
   var coords = function (value) {
      return A2(stringProperty,"coords",value);
   };
   var challenge = function (value) {
      return A2(stringProperty,"challenge",value);
   };
   var keytype = function (value) {
      return A2(stringProperty,"keytype",value);
   };
   var align = function (value) {
      return A2(stringProperty,"align",value);
   };
   var cite = function (value) {
      return A2(stringProperty,"cite",value);
   };
   var href = function (value) {
      return A2(stringProperty,"href",value);
   };
   var target = function (value) {
      return A2(stringProperty,"target",value);
   };
   var downloadAs = function (value) {
      return A2(stringProperty,"download",value);
   };
   var hreflang = function (value) {
      return A2(stringProperty,"hreflang",value);
   };
   var media = function (value) {
      return A2(stringProperty,"media",value);
   };
   var ping = function (value) {
      return A2(stringProperty,"ping",value);
   };
   var rel = function (value) {
      return A2(stringProperty,"rel",value);
   };
   var datetime = function (value) {
      return A2(stringProperty,"datetime",value);
   };
   var pubdate = function (value) {
      return A2(stringProperty,"pubdate",value);
   };
   var start = function (n) {
      return A2(stringProperty,"start",$Basics.toString(n));
   };
   var colspan = function (n) {
      return A2(stringProperty,"colSpan",$Basics.toString(n));
   };
   var headers = function (value) {
      return A2(stringProperty,"headers",value);
   };
   var rowspan = function (n) {
      return A2(stringProperty,"rowSpan",$Basics.toString(n));
   };
   var scope = function (value) {
      return A2(stringProperty,"scope",value);
   };
   var manifest = function (value) {
      return A2(stringProperty,"manifest",value);
   };
   var boolProperty = F2(function (name,bool) {
      return A2(property,name,$Json$Encode.bool(bool));
   });
   var hidden = function (bool) {
      return A2(boolProperty,"hidden",bool);
   };
   var contenteditable = function (bool) {
      return A2(boolProperty,"contentEditable",bool);
   };
   var spellcheck = function (bool) {
      return A2(boolProperty,"spellcheck",bool);
   };
   var async = function (bool) {
      return A2(boolProperty,"async",bool);
   };
   var defer = function (bool) {
      return A2(boolProperty,"defer",bool);
   };
   var scoped = function (bool) {
      return A2(boolProperty,"scoped",bool);
   };
   var autoplay = function (bool) {
      return A2(boolProperty,"autoplay",bool);
   };
   var controls = function (bool) {
      return A2(boolProperty,"controls",bool);
   };
   var loop = function (bool) {
      return A2(boolProperty,"loop",bool);
   };
   var $default = function (bool) {
      return A2(boolProperty,"default",bool);
   };
   var seamless = function (bool) {
      return A2(boolProperty,"seamless",bool);
   };
   var checked = function (bool) {
      return A2(boolProperty,"checked",bool);
   };
   var selected = function (bool) {
      return A2(boolProperty,"selected",bool);
   };
   var autofocus = function (bool) {
      return A2(boolProperty,"autofocus",bool);
   };
   var disabled = function (bool) {
      return A2(boolProperty,"disabled",bool);
   };
   var multiple = function (bool) {
      return A2(boolProperty,"multiple",bool);
   };
   var novalidate = function (bool) {
      return A2(boolProperty,"noValidate",bool);
   };
   var readonly = function (bool) {
      return A2(boolProperty,"readOnly",bool);
   };
   var required = function (bool) {
      return A2(boolProperty,"required",bool);
   };
   var ismap = function (value) {
      return A2(boolProperty,"isMap",value);
   };
   var download = function (bool) {
      return A2(boolProperty,"download",bool);
   };
   var reversed = function (bool) {
      return A2(boolProperty,"reversed",bool);
   };
   var classList = function (list) {
      return $class(A2($String.join,
      " ",
      A2($List.map,$Basics.fst,A2($List.filter,$Basics.snd,list))));
   };
   var style = function (props) {
      return A2(property,
      "style",
      $Json$Encode.object(A2($List.map,
      function (_p0) {
         var _p1 = _p0;
         return {ctor: "_Tuple2"
                ,_0: _p1._0
                ,_1: $Json$Encode.string(_p1._1)};
      },
      props)));
   };
   var key = function (k) {    return A2(stringProperty,"key",k);};
   return _elm.Html.Attributes.values = {_op: _op
                                        ,key: key
                                        ,style: style
                                        ,$class: $class
                                        ,classList: classList
                                        ,id: id
                                        ,title: title
                                        ,hidden: hidden
                                        ,type$: type$
                                        ,value: value
                                        ,checked: checked
                                        ,placeholder: placeholder
                                        ,selected: selected
                                        ,accept: accept
                                        ,acceptCharset: acceptCharset
                                        ,action: action
                                        ,autocomplete: autocomplete
                                        ,autofocus: autofocus
                                        ,autosave: autosave
                                        ,disabled: disabled
                                        ,enctype: enctype
                                        ,formaction: formaction
                                        ,list: list
                                        ,maxlength: maxlength
                                        ,minlength: minlength
                                        ,method: method
                                        ,multiple: multiple
                                        ,name: name
                                        ,novalidate: novalidate
                                        ,pattern: pattern
                                        ,readonly: readonly
                                        ,required: required
                                        ,size: size
                                        ,$for: $for
                                        ,form: form
                                        ,max: max
                                        ,min: min
                                        ,step: step
                                        ,cols: cols
                                        ,rows: rows
                                        ,wrap: wrap
                                        ,href: href
                                        ,target: target
                                        ,download: download
                                        ,downloadAs: downloadAs
                                        ,hreflang: hreflang
                                        ,media: media
                                        ,ping: ping
                                        ,rel: rel
                                        ,ismap: ismap
                                        ,usemap: usemap
                                        ,shape: shape
                                        ,coords: coords
                                        ,src: src
                                        ,height: height
                                        ,width: width
                                        ,alt: alt
                                        ,autoplay: autoplay
                                        ,controls: controls
                                        ,loop: loop
                                        ,preload: preload
                                        ,poster: poster
                                        ,$default: $default
                                        ,kind: kind
                                        ,srclang: srclang
                                        ,sandbox: sandbox
                                        ,seamless: seamless
                                        ,srcdoc: srcdoc
                                        ,reversed: reversed
                                        ,start: start
                                        ,align: align
                                        ,colspan: colspan
                                        ,rowspan: rowspan
                                        ,headers: headers
                                        ,scope: scope
                                        ,async: async
                                        ,charset: charset
                                        ,content: content
                                        ,defer: defer
                                        ,httpEquiv: httpEquiv
                                        ,language: language
                                        ,scoped: scoped
                                        ,accesskey: accesskey
                                        ,contenteditable: contenteditable
                                        ,contextmenu: contextmenu
                                        ,dir: dir
                                        ,draggable: draggable
                                        ,dropzone: dropzone
                                        ,itemprop: itemprop
                                        ,lang: lang
                                        ,spellcheck: spellcheck
                                        ,tabindex: tabindex
                                        ,challenge: challenge
                                        ,keytype: keytype
                                        ,cite: cite
                                        ,datetime: datetime
                                        ,pubdate: pubdate
                                        ,manifest: manifest
                                        ,property: property
                                        ,attribute: attribute};
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   if (_elm.Html.Events.values) return _elm.Html.Events.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var keyCode = A2($Json$Decode._op[":="],
   "keyCode",
   $Json$Decode.$int);
   var targetChecked = A2($Json$Decode.at,
   _U.list(["target","checked"]),
   $Json$Decode.bool);
   var targetValue = A2($Json$Decode.at,
   _U.list(["target","value"]),
   $Json$Decode.string);
   var defaultOptions = $VirtualDom.defaultOptions;
   var Options = F2(function (a,b) {
      return {stopPropagation: a,preventDefault: b};
   });
   var onWithOptions = $VirtualDom.onWithOptions;
   var on = $VirtualDom.on;
   var messageOn = F3(function (name,addr,msg) {
      return A3(on,
      name,
      $Json$Decode.value,
      function (_p0) {
         return A2($Signal.message,addr,msg);
      });
   });
   var onClick = messageOn("click");
   var onDoubleClick = messageOn("dblclick");
   var onMouseMove = messageOn("mousemove");
   var onMouseDown = messageOn("mousedown");
   var onMouseUp = messageOn("mouseup");
   var onMouseEnter = messageOn("mouseenter");
   var onMouseLeave = messageOn("mouseleave");
   var onMouseOver = messageOn("mouseover");
   var onMouseOut = messageOn("mouseout");
   var onBlur = messageOn("blur");
   var onFocus = messageOn("focus");
   var onSubmit = messageOn("submit");
   var onKey = F3(function (name,addr,handler) {
      return A3(on,
      name,
      keyCode,
      function (code) {
         return A2($Signal.message,addr,handler(code));
      });
   });
   var onKeyUp = onKey("keyup");
   var onKeyDown = onKey("keydown");
   var onKeyPress = onKey("keypress");
   return _elm.Html.Events.values = {_op: _op
                                    ,onBlur: onBlur
                                    ,onFocus: onFocus
                                    ,onSubmit: onSubmit
                                    ,onKeyUp: onKeyUp
                                    ,onKeyDown: onKeyDown
                                    ,onKeyPress: onKeyPress
                                    ,onClick: onClick
                                    ,onDoubleClick: onDoubleClick
                                    ,onMouseMove: onMouseMove
                                    ,onMouseDown: onMouseDown
                                    ,onMouseUp: onMouseUp
                                    ,onMouseEnter: onMouseEnter
                                    ,onMouseLeave: onMouseLeave
                                    ,onMouseOver: onMouseOver
                                    ,onMouseOut: onMouseOut
                                    ,on: on
                                    ,onWithOptions: onWithOptions
                                    ,defaultOptions: defaultOptions
                                    ,targetValue: targetValue
                                    ,targetChecked: targetChecked
                                    ,keyCode: keyCode
                                    ,Options: Options};
};
Elm.StartApp = Elm.StartApp || {};
Elm.StartApp.make = function (_elm) {
   "use strict";
   _elm.StartApp = _elm.StartApp || {};
   if (_elm.StartApp.values) return _elm.StartApp.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var start = function (config) {
      var updateStep = F2(function (action,_p0) {
         var _p1 = _p0;
         var _p2 = A2(config.update,action,_p1._0);
         var newModel = _p2._0;
         var additionalEffects = _p2._1;
         return {ctor: "_Tuple2"
                ,_0: newModel
                ,_1: $Effects.batch(_U.list([_p1._1,additionalEffects]))};
      });
      var update = F2(function (actions,_p3) {
         var _p4 = _p3;
         return A3($List.foldl,
         updateStep,
         {ctor: "_Tuple2",_0: _p4._0,_1: $Effects.none},
         actions);
      });
      var messages = $Signal.mailbox(_U.list([]));
      var singleton = function (action) {
         return _U.list([action]);
      };
      var address = A2($Signal.forwardTo,messages.address,singleton);
      var inputs = $Signal.mergeMany(A2($List._op["::"],
      messages.signal,
      A2($List.map,$Signal.map(singleton),config.inputs)));
      var effectsAndModel = A3($Signal.foldp,
      update,
      config.init,
      inputs);
      var model = A2($Signal.map,$Basics.fst,effectsAndModel);
      return {html: A2($Signal.map,config.view(address),model)
             ,model: model
             ,tasks: A2($Signal.map,
             function (_p5) {
                return A2($Effects.toTask,messages.address,$Basics.snd(_p5));
             },
             effectsAndModel)};
   };
   var App = F3(function (a,b,c) {
      return {html: a,model: b,tasks: c};
   });
   var Config = F4(function (a,b,c,d) {
      return {init: a,update: b,view: c,inputs: d};
   });
   return _elm.StartApp.values = {_op: _op
                                 ,start: start
                                 ,Config: Config
                                 ,App: App};
};
Elm.Ratio = Elm.Ratio || {};
Elm.Ratio.make = function (_elm) {
   "use strict";
   _elm.Ratio = _elm.Ratio || {};
   if (_elm.Ratio.values) return _elm.Ratio.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var toFloat = function (_p0) {
      var _p1 = _p0;
      return $Basics.toFloat(_p1._0) / $Basics.toFloat(_p1._1);
   };
   var split = function (_p2) {
      var _p3 = _p2;
      return {ctor: "_Tuple2",_0: _p3._0,_1: _p3._1};
   };
   var denominator = function (_p4) {
      var _p5 = _p4;
      return _p5._1;
   };
   var numerator = function (_p6) {
      var _p7 = _p6;
      return _p7._0;
   };
   var gcd = F2(function (a,b) {
      gcd: while (true) if (_U.eq(b,0)) return a; else {
            var _v4 = b,_v5 = A2($Basics._op["%"],a,b);
            a = _v4;
            b = _v5;
            continue gcd;
         }
   });
   var Ratio = F2(function (a,b) {
      return {ctor: "Ratio",_0: a,_1: b};
   });
   var normalize = function (_p8) {
      var _p9 = _p8;
      var _p11 = _p9._1;
      var _p10 = _p9._0;
      var k = A2(gcd,_p10,_p11) * (_U.cmp(_p11,0) < 0 ? -1 : 1);
      return A2(Ratio,_p10 / k | 0,_p11 / k | 0);
   };
   var add = F2(function (_p13,_p12) {
      var _p14 = _p13;
      var _p17 = _p14._1;
      var _p15 = _p12;
      var _p16 = _p15._1;
      return normalize(A2(Ratio,
      _p14._0 * _p16 + _p17 * _p15._0,
      _p17 * _p16));
   });
   var multiply = F2(function (_p19,_p18) {
      var _p20 = _p19;
      var _p21 = _p18;
      return normalize(A2(Ratio,_p20._0 * _p21._0,_p20._1 * _p21._1));
   });
   var divide = F2(function (r,_p22) {
      var _p23 = _p22;
      return A2(multiply,r,A2(Ratio,_p23._1,_p23._0));
   });
   var negate = function (_p24) {
      var _p25 = _p24;
      return A2(Ratio,0 - _p25._0,_p25._1);
   };
   var over = F2(function (x,y) {
      return normalize(A2(Ratio,x,y));
   });
   var fromInt = function (x) {    return A2(over,x,1);};
   return _elm.Ratio.values = {_op: _op
                              ,gcd: gcd
                              ,add: add
                              ,multiply: multiply
                              ,divide: divide
                              ,negate: negate
                              ,over: over
                              ,denominator: denominator
                              ,numerator: numerator
                              ,split: split
                              ,toFloat: toFloat
                              ,fromInt: fromInt};
};
Elm.Combine = Elm.Combine || {};
Elm.Combine.Extra = Elm.Combine.Extra || {};
Elm.Combine.Extra.make = function (_elm) {
   "use strict";
   _elm.Combine = _elm.Combine || {};
   _elm.Combine.Extra = _elm.Combine.Extra || {};
   if (_elm.Combine.Extra.values) return _elm.Combine.Extra.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Combine = Elm.Combine.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var leftBiasedOr = F2(function (lp,rp) {
      return $Combine.primitive(function (cx) {
         var res = A2($Combine.app,lp,cx);
         var _p0 = res;
         if (_p0._0.ctor === "Ok") {
               return res;
            } else {
               var res$ = A2($Combine.app,rp,cx);
               var _p1 = res$;
               if (_p1._0.ctor === "Ok") {
                     return res$;
                  } else {
                     var mcx = A2($Debug.log,"original or ctx",cx);
                     return {ctor: "_Tuple2"
                            ,_0: $Result.Err(A2($Basics._op["++"],_p0._0._0,_p1._0._0))
                            ,_1: A2($Debug.log,"left",_p0._1)};
                  }
            }
      });
   });
   var manyTill$ = F2(function (p,end) {
      var accumulate = F2(function (acc,cx) {
         accumulate: while (true) {
            var _p2 = A2($Combine.app,end,cx);
            if (_p2._0.ctor === "Ok") {
                  return {ctor: "_Tuple2"
                         ,_0: $Result.Ok($List.reverse(acc))
                         ,_1: _p2._1};
               } else {
                  var _p3 = A2($Combine.app,p,cx);
                  if (_p3._0.ctor === "Ok") {
                        var _v4 = A2($List._op["::"],_p3._0._0,acc),_v5 = _p3._1;
                        acc = _v4;
                        cx = _v5;
                        continue accumulate;
                     } else {
                        var mcx = A2($Debug.log,"original ctx",_p2._1);
                        return {ctor: "_Tuple2"
                               ,_0: $Result.Err(A2($Debug.log,"mt\' msg",_p3._0._0))
                               ,_1: A2($Debug.log,"mt\' ctx",_p3._1)};
                     }
               }
         }
      });
      return $Combine.primitive(accumulate(_U.list([])));
   });
   return _elm.Combine.Extra.values = {_op: _op
                                      ,manyTill$: manyTill$
                                      ,leftBiasedOr: leftBiasedOr};
};
Elm.Abc = Elm.Abc || {};
Elm.Abc.ParseTree = Elm.Abc.ParseTree || {};
Elm.Abc.ParseTree.make = function (_elm) {
   "use strict";
   _elm.Abc = _elm.Abc || {};
   _elm.Abc.ParseTree = _elm.Abc.ParseTree || {};
   if (_elm.Abc.ParseTree.values) return _elm.Abc.ParseTree.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var middlecOctave = 5;
   var UnsupportedHeader = {ctor: "UnsupportedHeader"};
   var Comment = function (a) {
      return {ctor: "Comment",_0: a};
   };
   var FieldContinuation = function (a) {
      return {ctor: "FieldContinuation",_0: a};
   };
   var Transcription = function (a) {
      return {ctor: "Transcription",_0: a};
   };
   var ReferenceNumber = function (a) {
      return {ctor: "ReferenceNumber",_0: a};
   };
   var WordsAligned = function (a) {
      return {ctor: "WordsAligned",_0: a};
   };
   var WordsAfter = function (a) {
      return {ctor: "WordsAfter",_0: a};
   };
   var Voice = function (a) {    return {ctor: "Voice",_0: a};};
   var UserDefined = function (a) {
      return {ctor: "UserDefined",_0: a};
   };
   var Title = function (a) {    return {ctor: "Title",_0: a};};
   var SymbolLine = function (a) {
      return {ctor: "SymbolLine",_0: a};
   };
   var Source = function (a) {    return {ctor: "Source",_0: a};};
   var Remark = function (a) {    return {ctor: "Remark",_0: a};};
   var Rhythm = function (a) {    return {ctor: "Rhythm",_0: a};};
   var Tempo = function (a) {    return {ctor: "Tempo",_0: a};};
   var Parts = function (a) {    return {ctor: "Parts",_0: a};};
   var Origin = function (a) {    return {ctor: "Origin",_0: a};};
   var Notes = function (a) {    return {ctor: "Notes",_0: a};};
   var Macro = function (a) {    return {ctor: "Macro",_0: a};};
   var Meter = function (a) {    return {ctor: "Meter",_0: a};};
   var UnitNoteLength = function (a) {
      return {ctor: "UnitNoteLength",_0: a};
   };
   var Key = function (a) {    return {ctor: "Key",_0: a};};
   var Instruction = function (a) {
      return {ctor: "Instruction",_0: a};
   };
   var History = function (a) {
      return {ctor: "History",_0: a};
   };
   var Group = function (a) {    return {ctor: "Group",_0: a};};
   var FileUrl = function (a) {
      return {ctor: "FileUrl",_0: a};
   };
   var Discography = function (a) {
      return {ctor: "Discography",_0: a};
   };
   var Composer = function (a) {
      return {ctor: "Composer",_0: a};
   };
   var Book = function (a) {    return {ctor: "Book",_0: a};};
   var Area = function (a) {    return {ctor: "Area",_0: a};};
   var RightArrow = function (a) {
      return {ctor: "RightArrow",_0: a};
   };
   var LeftArrow = function (a) {
      return {ctor: "LeftArrow",_0: a};
   };
   var TempoSignature = F3(function (a,b,c) {
      return {noteLengths: a,bpm: b,marking: c};
   });
   var KeyAccidental = F2(function (a,b) {
      return {pitchClass: a,accidental: b};
   });
   var KeySignature = F3(function (a,b,c) {
      return {pitchClass: a,accidental: b,mode: c};
   });
   var G = {ctor: "G"};
   var F = {ctor: "F"};
   var E = {ctor: "E"};
   var D = {ctor: "D"};
   var C = {ctor: "C"};
   var B = {ctor: "B"};
   var A = {ctor: "A"};
   var Natural = {ctor: "Natural"};
   var DoubleFlat = {ctor: "DoubleFlat"};
   var DoubleSharp = {ctor: "DoubleSharp"};
   var Flat = {ctor: "Flat"};
   var Sharp = {ctor: "Sharp"};
   var Locrian = {ctor: "Locrian"};
   var Aeolian = {ctor: "Aeolian"};
   var Mixolydian = {ctor: "Mixolydian"};
   var Lydian = {ctor: "Lydian"};
   var Phrygian = {ctor: "Phrygian"};
   var Dorian = {ctor: "Dorian"};
   var Ionian = {ctor: "Ionian"};
   var Minor = {ctor: "Minor"};
   var Major = {ctor: "Major"};
   var Bar = F3(function (a,b,c) {
      return {lines: a,repeat: b,iteration: c};
   });
   var BeginAndEnd = {ctor: "BeginAndEnd"};
   var End = {ctor: "End"};
   var Begin = {ctor: "Begin"};
   var Continuation = {ctor: "Continuation"};
   var Ignore = {ctor: "Ignore"};
   var Spacer = function (a) {    return {ctor: "Spacer",_0: a};};
   var NoteSequence = function (a) {
      return {ctor: "NoteSequence",_0: a};
   };
   var Inline = function (a) {    return {ctor: "Inline",_0: a};};
   var Chord = function (a) {    return {ctor: "Chord",_0: a};};
   var ChordSymbol = function (a) {
      return {ctor: "ChordSymbol",_0: a};
   };
   var Annotation = F2(function (a,b) {
      return {ctor: "Annotation",_0: a,_1: b};
   });
   var GraceNote = F2(function (a,b) {
      return {ctor: "GraceNote",_0: a,_1: b};
   });
   var Slur = function (a) {    return {ctor: "Slur",_0: a};};
   var Decoration = function (a) {
      return {ctor: "Decoration",_0: a};
   };
   var Tuplet = F2(function (a,b) {
      return {ctor: "Tuplet",_0: a,_1: b};
   });
   var Rest = function (a) {    return {ctor: "Rest",_0: a};};
   var BrokenRhythmPair = F3(function (a,b,c) {
      return {ctor: "BrokenRhythmPair",_0: a,_1: b,_2: c};
   });
   var Note = function (a) {    return {ctor: "Note",_0: a};};
   var Barline = function (a) {
      return {ctor: "Barline",_0: a};
   };
   var Discretional = {ctor: "Discretional"};
   var RightOfNextSymbol = {ctor: "RightOfNextSymbol"};
   var LeftOfNextSymbol = {ctor: "LeftOfNextSymbol"};
   var BelowNextSymbol = {ctor: "BelowNextSymbol"};
   var AboveNextSymbol = {ctor: "AboveNextSymbol"};
   var AbcChord = F3(function (a,b,c) {
      return {notes: a,accidental: b,duration: c};
   });
   var AbcNote = F5(function (a,b,c,d,e) {
      return {pitchClass: a
             ,accidental: b
             ,octave: c
             ,duration: d
             ,tied: e};
   });
   var BodyInfo = function (a) {
      return {ctor: "BodyInfo",_0: a};
   };
   var Score = function (a) {    return {ctor: "Score",_0: a};};
   return _elm.Abc.ParseTree.values = {_op: _op
                                      ,middlecOctave: middlecOctave
                                      ,AbcNote: AbcNote
                                      ,AbcChord: AbcChord
                                      ,Bar: Bar
                                      ,KeySignature: KeySignature
                                      ,KeyAccidental: KeyAccidental
                                      ,TempoSignature: TempoSignature
                                      ,Score: Score
                                      ,BodyInfo: BodyInfo
                                      ,Area: Area
                                      ,Book: Book
                                      ,Composer: Composer
                                      ,Discography: Discography
                                      ,FileUrl: FileUrl
                                      ,Group: Group
                                      ,History: History
                                      ,Instruction: Instruction
                                      ,Key: Key
                                      ,UnitNoteLength: UnitNoteLength
                                      ,Meter: Meter
                                      ,Macro: Macro
                                      ,Notes: Notes
                                      ,Origin: Origin
                                      ,Parts: Parts
                                      ,Tempo: Tempo
                                      ,Rhythm: Rhythm
                                      ,Remark: Remark
                                      ,Source: Source
                                      ,SymbolLine: SymbolLine
                                      ,Title: Title
                                      ,UserDefined: UserDefined
                                      ,Voice: Voice
                                      ,WordsAfter: WordsAfter
                                      ,WordsAligned: WordsAligned
                                      ,ReferenceNumber: ReferenceNumber
                                      ,Transcription: Transcription
                                      ,FieldContinuation: FieldContinuation
                                      ,Comment: Comment
                                      ,UnsupportedHeader: UnsupportedHeader
                                      ,Barline: Barline
                                      ,Note: Note
                                      ,BrokenRhythmPair: BrokenRhythmPair
                                      ,Rest: Rest
                                      ,Tuplet: Tuplet
                                      ,Decoration: Decoration
                                      ,Slur: Slur
                                      ,GraceNote: GraceNote
                                      ,Annotation: Annotation
                                      ,ChordSymbol: ChordSymbol
                                      ,Chord: Chord
                                      ,Inline: Inline
                                      ,NoteSequence: NoteSequence
                                      ,Spacer: Spacer
                                      ,Ignore: Ignore
                                      ,Continuation: Continuation
                                      ,Begin: Begin
                                      ,End: End
                                      ,BeginAndEnd: BeginAndEnd
                                      ,AboveNextSymbol: AboveNextSymbol
                                      ,BelowNextSymbol: BelowNextSymbol
                                      ,LeftOfNextSymbol: LeftOfNextSymbol
                                      ,RightOfNextSymbol: RightOfNextSymbol
                                      ,Discretional: Discretional
                                      ,Major: Major
                                      ,Minor: Minor
                                      ,Ionian: Ionian
                                      ,Dorian: Dorian
                                      ,Phrygian: Phrygian
                                      ,Lydian: Lydian
                                      ,Mixolydian: Mixolydian
                                      ,Aeolian: Aeolian
                                      ,Locrian: Locrian
                                      ,Sharp: Sharp
                                      ,Flat: Flat
                                      ,DoubleSharp: DoubleSharp
                                      ,DoubleFlat: DoubleFlat
                                      ,Natural: Natural
                                      ,A: A
                                      ,B: B
                                      ,C: C
                                      ,D: D
                                      ,E: E
                                      ,F: F
                                      ,G: G
                                      ,LeftArrow: LeftArrow
                                      ,RightArrow: RightArrow};
};
Elm.Abc = Elm.Abc || {};
Elm.Abc.make = function (_elm) {
   "use strict";
   _elm.Abc = _elm.Abc || {};
   if (_elm.Abc.values) return _elm.Abc.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Abc$ParseTree = Elm.Abc.ParseTree.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Combine = Elm.Combine.make(_elm),
   $Combine$Char = Elm.Combine.Char.make(_elm),
   $Combine$Extra = Elm.Combine.Extra.make(_elm),
   $Combine$Infix = Elm.Combine.Infix.make(_elm),
   $Combine$Num = Elm.Combine.Num.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Maybe$Extra = Elm.Maybe.Extra.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var parseError = function (pe) {
      var append = F2(function (a,b) {
         return A2($Basics._op["++"],a,A2($Basics._op["++"],",",b));
      });
      var msg = A3($List.foldr,append,"",pe.msgs);
      return A2($Basics._op["++"],
      "parse error: ",
      A2($Basics._op["++"],
      msg,
      A2($Basics._op["++"],
      " on ",
      A2($Basics._op["++"],
      pe.input,
      A2($Basics._op["++"],
      " at position ",
      $Basics.toString(pe.position))))));
   };
   var toTupletInt = function (s) {
      return A2($Maybe.withDefault,
      3,
      $Result.toMaybe($String.toInt(s)));
   };
   var buildAnnotation = function (s) {
      var firstChar = $List.head($String.toList(s));
      var placement = function () {
         var _p0 = firstChar;
         _v0_4: do {
            if (_p0.ctor === "Just") {
                  switch (_p0._0.valueOf())
                  {case "^": return $Abc$ParseTree.AboveNextSymbol;
                     case "_": return $Abc$ParseTree.BelowNextSymbol;
                     case "<": return $Abc$ParseTree.LeftOfNextSymbol;
                     case ">": return $Abc$ParseTree.RightOfNextSymbol;
                     default: break _v0_4;}
               } else {
                  break _v0_4;
               }
         } while (false);
         return $Abc$ParseTree.Discretional;
      }();
      return A2($Abc$ParseTree.Annotation,placement,s);
   };
   var buildBrokenOperator = function (s) {
      return A2($String.startsWith,
      "<",
      s) ? $Abc$ParseTree.LeftArrow($String.length(s)) : $Abc$ParseTree.RightArrow($String.length(s));
   };
   var buildTupletSignature = F3(function (ps,mq,mr) {
      var p = toTupletInt(ps);
      var qdefault = function () {
         var _p1 = p;
         switch (_p1)
         {case 2: return 3;
            case 3: return 2;
            case 4: return 3;
            case 6: return 2;
            case 8: return 3;
            default: return 2;}
      }();
      var q = A2($Maybe.withDefault,
      qdefault,
      A2($Maybe.map,toTupletInt,mq));
      var r = A2($Maybe.withDefault,p,A2($Maybe.map,toTupletInt,mr));
      return {ctor: "_Tuple3",_0: p,_1: q,_2: r};
   });
   var buildChord = F3(function (macc,ns,ml) {
      var l = A2($Maybe.withDefault,$Ratio.fromInt(1),ml);
      return {notes: ns,accidental: macc,duration: l};
   });
   var buildAccidental = function (s) {
      var _p2 = s;
      switch (_p2)
      {case "^^": return $Abc$ParseTree.DoubleSharp;
         case "__": return $Abc$ParseTree.DoubleFlat;
         case "^": return $Abc$ParseTree.Sharp;
         case "_": return $Abc$ParseTree.Flat;
         default: return $Abc$ParseTree.Natural;}
   };
   var scientificPitchNotation = F2(function (pc,oct) {
      return A2($Regex.contains,
      $Regex.regex("[A-G]"),
      pc) ? $Abc$ParseTree.middlecOctave + oct : $Abc$ParseTree.middlecOctave + 1 + oct;
   });
   var buildBarline = F2(function (s,i) {
      var f = function (c) {
         var _p3 = c;
         switch (_p3.valueOf())
         {case "[": return _U.chr("|");
            case "]": return _U.chr("|");
            default: return c;}
      };
      var normalised = A2($String.map,f,s);
      var lines = $String.length(A2($String.filter,
      function (c) {
         return _U.eq(c,_U.chr("|"));
      },
      normalised));
      var normalisedLineCount = A2($Basics.max,
      A2($Basics.min,lines,2),
      1);
      var repeatCount = $String.length(A2($String.filter,
      function (c) {
         return _U.eq(c,_U.chr(":"));
      },
      normalised));
      var repeat = _U.eq(repeatCount,
      0) ? $Maybe.Nothing : _U.eq(repeatCount,
      1) ? A2($String.contains,
      ":|",
      normalised) ? $Maybe.Just($Abc$ParseTree.End) : $Maybe.Just($Abc$ParseTree.Begin) : $Maybe.Just($Abc$ParseTree.BeginAndEnd);
      return $Abc$ParseTree.Barline({lines: normalisedLineCount
                                    ,repeat: repeat
                                    ,iteration: i});
   });
   var buildKey = F3(function (c,ks,ka) {
      return $Abc$ParseTree.Key({ctor: "_Tuple2",_0: ks,_1: ka});
   });
   var pitchClassDict = $Dict.fromList(_U.list([{ctor: "_Tuple2"
                                                ,_0: "A"
                                                ,_1: $Abc$ParseTree.A}
                                               ,{ctor: "_Tuple2",_0: "B",_1: $Abc$ParseTree.B}
                                               ,{ctor: "_Tuple2",_0: "C",_1: $Abc$ParseTree.C}
                                               ,{ctor: "_Tuple2",_0: "D",_1: $Abc$ParseTree.D}
                                               ,{ctor: "_Tuple2",_0: "E",_1: $Abc$ParseTree.E}
                                               ,{ctor: "_Tuple2",_0: "F",_1: $Abc$ParseTree.F}
                                               ,{ctor: "_Tuple2",_0: "G",_1: $Abc$ParseTree.G}]));
   var lookupPitch = function (p) {
      return A2($Maybe.withDefault,
      $Abc$ParseTree.C,
      A2($Dict.get,$String.toUpper(p),pitchClassDict));
   };
   var buildKeySignature = F3(function (pStr,ma,mm) {
      return {pitchClass: lookupPitch(pStr)
             ,accidental: ma
             ,mode: A2($Maybe.withDefault,$Abc$ParseTree.Major,mm)};
   });
   var buildNote = F5(function (macc,pitchStr,octave,ml,mt) {
      var tied = function () {
         var _p4 = mt;
         if (_p4.ctor === "Just") {
               return true;
            } else {
               return false;
            }
      }();
      var spn = A2(scientificPitchNotation,pitchStr,octave);
      var p = lookupPitch($String.toUpper(pitchStr));
      var l = A2($Maybe.withDefault,$Ratio.fromInt(1),ml);
      return {pitchClass: p
             ,accidental: macc
             ,octave: spn
             ,duration: l
             ,tied: tied};
   });
   var buildKeyAccidental = F2(function (a,pitchStr) {
      var pc = lookupPitch(pitchStr);
      return {accidental: a,pitchClass: pc};
   });
   var buildTempoSignature = F5(function (ms1,fs,c,i,ms2) {
      var ms = function () {
         var _p5 = ms1;
         if (_p5.ctor === "Nothing") {
               return ms2;
            } else {
               return ms1;
            }
      }();
      return {noteLengths: fs,bpm: i,marking: ms};
   });
   var buildRationalFromExponential = function (i) {
      return A2($Ratio.over,1,Math.pow(2,i));
   };
   var tup = A2($Combine$Infix._op["<$>"],
   $Maybe$Extra.join,
   $Combine.maybe(A2($Combine$Infix._op["*>"],
   $Combine$Char.$char(_U.chr(":")),
   $Combine.maybe($Combine.regex("[2-9]")))));
   var tupletSignature = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],
   buildTupletSignature,
   $Combine.regex("[2-9]")),
   tup),
   tup);
   var integralAsRational = A2($Combine$Infix._op["<$>"],
   $Ratio.fromInt,
   $Combine$Num.digit);
   var maybeTie = A2($Combine$Infix._op["<?>"],
   $Combine.maybe($Combine$Char.$char(_U.chr("-"))),
   "tie");
   var octaveShift = function (s) {
      var f = F2(function (c,acc) {
         var _p6 = c;
         switch (_p6.valueOf())
         {case "\'": var _p7 = acc;
              var up = _p7._0;
              var down = _p7._1;
              return {ctor: "_Tuple2",_0: up + 1,_1: down};
            case ",": var _p8 = acc;
              var up = _p8._0;
              var down = _p8._1;
              return {ctor: "_Tuple2",_0: up,_1: down + 1};
            default: return acc;}
      });
      var octs = A3($String.foldl,f,{ctor: "_Tuple2",_0: 0,_1: 0},s);
      return $Basics.fst(octs) - $Basics.snd(octs);
   };
   var moveOctave = A2($Combine$Infix._op["<$>"],
   octaveShift,
   $Combine.regex("[\',]*"));
   var accidental = A2($Combine$Infix._op["<$>"],
   buildAccidental,
   $Combine.choice(_U.list([$Combine.string("^^")
                           ,$Combine.string("__")
                           ,$Combine.string("^")
                           ,$Combine.string("_")
                           ,$Combine.string("=")])));
   var maybeAccidental = $Combine.maybe(accidental);
   var pitch = $Combine.regex("[A-Ga-g]");
   var inlineInfo = function (isInline) {
      var pattern = isInline ? "[^\r\n\\[\\]]*" : "[^\r\n]*";
      return $Combine.regex(pattern);
   };
   var strToEol = $Combine.regex("[^\r\n]*");
   var annotationString = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["*>"],
   $Combine.string("\""),
   $Combine.regex("[\\^\\>\\<-@](\\\\\"|[^\"\n])*")),
   $Combine.string("\"")),
   "annotation");
   var quotedString = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["*>"],
   $Combine.string("\""),
   $Combine.regex("(\\\\\"|[^\"\n])*")),
   $Combine.string("\"")),
   "quoted string");
   var continuation = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<*"],
   $Combine.succeed($Abc$ParseTree.Continuation),
   $Combine$Char.$char(_U.chr("\\"))),
   $Combine.regex("[^\r\n]*")),
   "continuation");
   var ignore = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*"],
   $Combine.succeed($Abc$ParseTree.Ignore),
   $Combine.regex("[#@;`\\*\\?]+")),
   "ignored character");
   var scoreSpace = $Combine.choice(_U.list([$Combine$Char.space
                                            ,$Combine$Char.$char(_U.chr("y"))
                                            ,$Combine$Char.tab]));
   var spacer = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Spacer,
   A2($Combine$Infix._op["<$>"],
   $List.length,
   $Combine.many1(scoreSpace))),
   "space");
   var whiteSpace = A2($Combine$Infix._op["<$>"],
   $String.fromList,
   $Combine.many($Combine.choice(_U.list([$Combine$Char.space
                                         ,$Combine$Char.tab]))));
   var headerCode = function (c) {
      return A2($Combine$Infix._op["<*"],
      A2($Combine$Infix._op["<*"],
      $Combine$Char.$char(c),
      $Combine$Char.$char(_U.chr(":"))),
      whiteSpace);
   };
   var unsupportedHeaderCode = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<*"],
   $Combine.regex("[a-qt-vx-zEJ]"),
   $Combine$Char.$char(_U.chr(":"))),
   whiteSpace);
   var spacedQuotedString = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["*>"],whiteSpace,quotedString),
   whiteSpace);
   var comment = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Comment,
   A2($Combine$Infix._op["*>"],$Combine.regex("%"),strToEol)),
   "comment");
   var unsupportedHeader = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],
   $Abc$ParseTree.UnsupportedHeader,
   unsupportedHeaderCode),
   strToEol),
   "unsupported header");
   var fieldContinuation = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.FieldContinuation,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("+")),strToEol)),
   "field continuation");
   var transcription = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Transcription,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("Z")),strToEol)),
   "Z header");
   var referenceNumber = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.ReferenceNumber,
   A2($Combine$Infix._op["*>"],
   headerCode(_U.chr("X")),
   $Combine$Num.$int)),
   "x header");
   var wordsAligned = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.WordsAligned,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("w")),
      inlineInfo(isInline))),
      "w header");
   };
   var wordsAfter = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.WordsAfter,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("W")),
      inlineInfo(isInline))),
      "W header");
   };
   var voice = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Voice,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("V")),
      inlineInfo(isInline))),
      "V header");
   };
   var userDefined = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.UserDefined,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("U")),
      inlineInfo(isInline))),
      "U header");
   };
   var title = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Title,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("T")),
      inlineInfo(isInline))),
      "T header");
   };
   var symbolLine = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.SymbolLine,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("s")),
      inlineInfo(isInline))),
      "s header");
   };
   var tuneBodyOnlyInfo = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      $Combine.choice(_U.list([symbolLine(isInline)
                              ,wordsAligned(isInline)])),
      "tune body only info");
   };
   var source = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Source,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("S")),strToEol)),
   "S header");
   var remark = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Remark,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("r")),
      inlineInfo(isInline))),
      "r header");
   };
   var rhythm = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Rhythm,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("R")),
      inlineInfo(isInline))),
      "R header");
   };
   var parts = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Parts,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("P")),
      inlineInfo(isInline))),
      "P header");
   };
   var origin = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Origin,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("O")),strToEol)),
   "O header");
   var notes = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Notes,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("N")),
      inlineInfo(isInline))),
      "N header");
   };
   var macro = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Macro,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("m")),
      inlineInfo(isInline))),
      "m header");
   };
   var instruction = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Abc$ParseTree.Instruction,
      A2($Combine$Infix._op["*>"],
      headerCode(_U.chr("I")),
      inlineInfo(isInline))),
      "I header");
   };
   var history = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.History,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("H")),strToEol)),
   "H header");
   var group = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Group,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("G")),strToEol)),
   "G header");
   var fileUrl = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.FileUrl,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("F")),strToEol)),
   "F header");
   var discography = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Discography,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("D")),strToEol)),
   "D header");
   var composer = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Composer,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("C")),strToEol)),
   "C header");
   var book = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Book,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("B")),strToEol)),
   "B Header");
   var area = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Area,
   A2($Combine$Infix._op["*>"],headerCode(_U.chr("A")),strToEol)),
   "A header");
   var tuneInfo = A2($Combine$Infix._op["<?>"],
   $Combine.choice(_U.list([area
                           ,book
                           ,composer
                           ,discography
                           ,fileUrl
                           ,group
                           ,history
                           ,origin
                           ,source
                           ,referenceNumber
                           ,transcription
                           ,unsupportedHeader])),
   "tune info");
   var locrian = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Locrian,whiteSpace),
   $Combine.regex("(L|l)(O|o)(C|c)([A-Za-z])*"));
   var aeolian = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Aeolian,whiteSpace),
   $Combine.regex("(A|a)(E|e)(O|o)([A-Za-z])*"));
   var mixolydian = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],
   $Abc$ParseTree.Mixolydian,
   whiteSpace),
   $Combine.regex("(M|m)(I|i)(X|x)([A-Za-z])*"));
   var lydian = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Lydian,whiteSpace),
   $Combine.regex("(L|l)(Y|y)(D|d)([A-Za-z])*"));
   var phrygian = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Phrygian,whiteSpace),
   $Combine.regex("(P|p)(H|h)(R|r)([A-Za-z])*"));
   var dorian = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Dorian,whiteSpace),
   $Combine.regex("(D|d)(O|o)(R|r)([A-Za-z])*"));
   var ionian = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Ionian,whiteSpace),
   $Combine.regex("(I|i)(O|o)(N|n)([A-Za-z])*"));
   var major = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Major,whiteSpace),
   $Combine.regex("(M|m)(A|a)(J|j)([A-Za-z])*"));
   var minor = A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$"],$Abc$ParseTree.Minor,whiteSpace),
   $Combine.regex("(M|m)([A-Za-z])*"));
   var mode = $Combine.choice(_U.list([major
                                      ,ionian
                                      ,dorian
                                      ,phrygian
                                      ,lydian
                                      ,mixolydian
                                      ,aeolian
                                      ,locrian
                                      ,minor]));
   var keyAccidental = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],buildKeyAccidental,accidental),
   pitch);
   var keyAccidentals = $Combine.many(A2($Combine$Infix._op["*>"],
   $Combine$Char.space,
   keyAccidental));
   var keyName = $Combine.regex("[A-G]");
   var sharpOrFlat = A2($Combine.map,
   function (x) {
      return _U.eq(x,
      _U.chr("#")) ? $Abc$ParseTree.Sharp : $Abc$ParseTree.Flat;
   },
   $Combine.choice(_U.list([$Combine$Char.$char(_U.chr("#"))
                           ,$Combine$Char.$char(_U.chr("b"))])));
   var keySignature = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],buildKeySignature,keyName),
   $Combine.maybe(sharpOrFlat)),
   $Combine.maybe(mode));
   var key = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],buildKey,headerCode(_U.chr("K"))),
   keySignature),
   keyAccidentals),
   strToEol),
   "K header");
   var nometer = A2($Combine$Infix._op["<$"],
   $Maybe.Nothing,
   $Combine.string("none"));
   var meterSignature = A2($Combine$Infix._op["<$>"],
   $Maybe.Just,
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$>"],
   F2(function (v0,v1) {
      return {ctor: "_Tuple2",_0: v0,_1: v1};
   }),
   $Combine$Num.$int),
   $Combine$Char.$char(_U.chr("/"))),
   $Combine$Num.$int),
   whiteSpace));
   var cutTime = A2($Combine$Infix._op["<$"],
   $Maybe.Just({ctor: "_Tuple2",_0: 2,_1: 2}),
   $Combine.string("C|"));
   var commonTime = A2($Combine$Infix._op["<$"],
   $Maybe.Just({ctor: "_Tuple2",_0: 4,_1: 4}),
   $Combine$Char.$char(_U.chr("C")));
   var meterDefinition = $Combine.choice(_U.list([cutTime
                                                 ,commonTime
                                                 ,meterSignature
                                                 ,nometer]));
   var meter = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Meter,
   A2($Combine$Infix._op["*>"],
   headerCode(_U.chr("M")),
   meterDefinition)),
   "M header");
   var slashesRational = A2($Combine$Infix._op["<$>"],
   buildRationalFromExponential,
   A2($Combine$Infix._op["<$>"],
   $List.length,
   $Combine.many1($Combine$Char.$char(_U.chr("/")))));
   var curtailedRational = A2($Combine$Infix._op["<$>"],
   $Ratio.over(1),
   A2($Combine$Infix._op["*>"],
   $Combine$Char.$char(_U.chr("/")),
   $Combine$Num.$int));
   var rational = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$>"],$Ratio.over,$Combine$Num.$int),
   $Combine$Char.$char(_U.chr("/"))),
   $Combine$Num.$int);
   var headerRational = A2($Combine$Infix._op["<*"],
   rational,
   whiteSpace);
   var tempoSignature = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],
   buildTempoSignature,
   $Combine.maybe(spacedQuotedString)),
   $Combine.many(headerRational)),
   $Combine.maybe($Combine$Char.$char(_U.chr("=")))),
   $Combine$Num.$int),
   $Combine.maybe(spacedQuotedString));
   var tempo = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Tempo,
   A2($Combine$Infix._op["*>"],
   headerCode(_U.chr("Q")),
   tempoSignature)),
   "Q header");
   var noteDuration = A2($Combine$Infix._op["<*"],
   rational,
   whiteSpace);
   var unitNoteLength = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.UnitNoteLength,
   A2($Combine$Infix._op["*>"],
   headerCode(_U.chr("L")),
   noteDuration)),
   "L header");
   var anywhereInfo = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      $Combine.choice(_U.list([instruction(isInline)
                              ,key
                              ,unitNoteLength
                              ,meter
                              ,macro(isInline)
                              ,notes(isInline)
                              ,parts(isInline)
                              ,tempo
                              ,rhythm(isInline)
                              ,remark(isInline)
                              ,title(isInline)
                              ,userDefined(isInline)
                              ,voice(isInline)
                              ,wordsAfter(isInline)
                              ,fieldContinuation
                              ,comment])),
      "anywhere info");
   };
   var informationField = function (isInline) {
      return A2($Combine$Infix._op["<$>"],
      $Debug.log("header"),
      A2($Combine$Infix._op["<?>"],
      $Combine.choice(_U.list([anywhereInfo(isInline),tuneInfo])),
      "header"));
   };
   var header = A2($Combine$Infix._op["<*"],
   informationField(false),
   $Combine$Char.eol);
   var headers = A2($Combine$Infix._op["<?>"],
   $Combine.many(header),
   "headers");
   var tuneBodyInfo = function (isInline) {
      return A2($Combine$Infix._op["<?>"],
      $Combine.choice(_U.list([tuneBodyOnlyInfo(isInline)
                              ,anywhereInfo(isInline)])),
      "tune body info");
   };
   var tuneBodyHeader = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.BodyInfo,
   tuneBodyInfo(true)),
   $Combine$Char.eol),
   "tune body header");
   var noteDur = $Combine.choice(_U.list([rational
                                         ,integralAsRational
                                         ,curtailedRational
                                         ,slashesRational]));
   var abcNote = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],buildNote,maybeAccidental),
   pitch),
   moveOctave),
   $Combine.maybe(noteDur)),
   maybeTie),
   "ABC note");
   var note = A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Note,
   abcNote);
   var tuplet = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Tuplet,
   A2($Combine$Infix._op["*>"],
   $Combine$Char.$char(_U.chr("(")),
   tupletSignature)),
   $Combine.many1(abcNote)),
   "tuplet");
   var abcChord = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],buildChord,maybeAccidental),
   A3($Combine.between,
   $Combine$Char.$char(_U.chr("[")),
   $Combine$Char.$char(_U.chr("]")),
   $Combine.many1(abcNote))),
   $Combine.maybe(noteDur)),
   "ABC chord");
   var noteSequence = A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.NoteSequence,
   $Combine.many1(note));
   var longDecoration = A2($Combine$Infix._op["<?>"],
   A3($Combine.between,
   $Combine$Char.$char(_U.chr("!")),
   $Combine$Char.$char(_U.chr("!")),
   $Combine.regex("[^\r\n!]*")),
   "long decoration");
   var shortDecoration = A2($Combine$Infix._op["<?>"],
   $Combine.regex("[\\.~HLMOPSTuv]"),
   "short decoration");
   var decoration = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Decoration,
   $Combine.choice(_U.list([shortDecoration,longDecoration]))),
   "decoration");
   var acciaccatura = A2($Combine$Infix._op["<$>"],
   function (_p9) {
      return true;
   },
   $Combine.maybe($Combine$Char.$char(_U.chr("/"))));
   var inline = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Inline,
   A3($Combine.between,
   $Combine$Char.$char(_U.chr("[")),
   $Combine$Char.$char(_U.chr("]")),
   tuneBodyInfo(true))),
   "inline header");
   var chord = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],$Abc$ParseTree.Chord,abcChord),
   "chord");
   var grace = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.GraceNote,
   acciaccatura),
   $Combine.choice(_U.list([noteSequence,chord])));
   var graceNote = A2($Combine$Infix._op["<?>"],
   A3($Combine.between,
   $Combine$Char.$char(_U.chr("{")),
   $Combine$Char.$char(_U.chr("}")),
   grace),
   "grace note");
   var annotation = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],buildAnnotation,annotationString),
   "annotation");
   var chordSymbol = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.ChordSymbol,
   quotedString),
   "chord symbol");
   var rest = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Rest,
   A2($Combine$Infix._op["<$>"],
   $Maybe.withDefault($Ratio.fromInt(1)),
   A2($Combine$Infix._op["*>"],
   $Combine.regex("[XxZz]"),
   $Combine.maybe(noteDur)))),
   "rest");
   var brokenRhythmTie = A2($Combine$Infix._op["<$>"],
   buildBrokenOperator,
   $Combine.regex("(<+|>+)"));
   var brokenRhythmPair = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.BrokenRhythmPair,
   abcNote),
   brokenRhythmTie),
   abcNote),
   "broken rhythm pair");
   var slur = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Slur,
   $Combine.choice(_U.list([$Combine$Char.$char(_U.chr("("))
                           ,$Combine$Char.$char(_U.chr(")"))]))),
   "slur");
   var barSeparator = A2($Combine$Infix._op["<$>"],
   $String.fromList,
   $Combine.many1($Combine.choice(_U.list([$Combine$Char.$char(_U.chr("|"))
                                          ,$Combine$Char.$char(_U.chr("["))
                                          ,$Combine$Char.$char(_U.chr("]"))
                                          ,$Combine$Char.$char(_U.chr(":"))]))));
   var barline = A2($Combine$Infix._op["<?>"],
   A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],buildBarline,barSeparator),
   $Combine.maybe($Combine$Num.digit)),
   "barline");
   var scoreItem = $Combine.rec(function (_p10) {
      var _p11 = _p10;
      return A2($Combine$Infix._op["<?>"],
      A2($Combine$Infix._op["<$>"],
      $Debug.log("score item"),
      $Combine.choice(_U.list([chord
                              ,inline
                              ,barline
                              ,brokenRhythmPair
                              ,note
                              ,rest
                              ,tuplet
                              ,slur
                              ,graceNote
                              ,annotation
                              ,chordSymbol
                              ,decoration
                              ,spacer
                              ,ignore
                              ,continuation]))),
      "score item");
   });
   var score = A2($Combine$Infix._op["<$>"],
   $Abc$ParseTree.Score,
   A2($Combine$Extra.manyTill$,scoreItem,$Combine$Char.eol));
   var body = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],
   F2(function (x,y) {    return A2($List._op["::"],x,y);}),
   score),
   A2($Combine$Extra.manyTill$,
   A2($Combine$Extra.leftBiasedOr,score,tuneBodyHeader),
   $Combine.end));
   var abc = A2($Combine$Infix._op["<*>"],
   A2($Combine$Infix._op["<$>"],
   F2(function (v0,v1) {
      return {ctor: "_Tuple2",_0: v0,_1: v1};
   }),
   headers),
   body);
   var parse = function (s) {
      var _p12 = A2($Combine.parse,abc,s);
      if (_p12._0.ctor === "Ok") {
            return $Result.Ok(_p12._0._0);
         } else {
            var _p13 = _p12._1;
            return $Result.Err({msgs: _p12._0._0
                               ,input: _p13.input
                               ,position: _p13.position});
         }
   };
   var ParseError = F3(function (a,b,c) {
      return {msgs: a,input: b,position: c};
   });
   return _elm.Abc.values = {_op: _op
                            ,parse: parse
                            ,parseError: parseError
                            ,ParseError: ParseError};
};
Elm.Music = Elm.Music || {};
Elm.Music.Notation = Elm.Music.Notation || {};
Elm.Music.Notation.make = function (_elm) {
   "use strict";
   _elm.Music = _elm.Music || {};
   _elm.Music.Notation = _elm.Music.Notation || {};
   if (_elm.Music.Notation.values)
   return _elm.Music.Notation.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Abc$ParseTree = Elm.Abc.ParseTree.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm),
   $List$Extra = Elm.List.Extra.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Maybe$Extra = Elm.Maybe.Extra.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var isFlatMajorKey = function (target) {
      var _p0 = target;
      var pc = _p0._0;
      var accidental = _p0._1;
      var _p1 = accidental;
      if (_p1.ctor === "Nothing") {
            return _U.eq(pc,$Abc$ParseTree.F);
         } else {
            return _U.eq(_p1._0,$Abc$ParseTree.Flat);
         }
   };
   var accidentalKey = function (k) {
      var _p2 = k;
      var pc = _p2._0;
      var acc = _p2._1;
      var _p3 = acc;
      if (_p3.ctor === "Nothing") {
            return false;
         } else {
            return true;
         }
   };
   var lookUp = F2(function (s,i) {
      return A2($Maybe.withDefault,
      {ctor: "_Tuple2",_0: $Abc$ParseTree.C,_1: $Maybe.Nothing},
      A2($List$Extra.getAt,s,i));
   });
   var partialSum = function (l) {
      return A2($List.take,
      $List.length(l),
      $List.reverse(A2($List.map,
      $List.sum,
      $List$Extra.tails($List.reverse(l)))));
   };
   var rotateLeftBy = F2(function (index,ls) {
      var listPair = A2($List$Extra.splitAt,index,ls);
      return A2($List.append,
      $Basics.snd(listPair),
      $Basics.fst(listPair));
   });
   var rotateFrom = F2(function (target,scale) {
      var index = A2($Maybe.withDefault,
      0,
      A2($List$Extra.elemIndex,target,scale));
      var listPair = A2($List$Extra.splitAt,index,scale);
      return A2($List.append,
      $Basics.snd(listPair),
      $Basics.fst(listPair));
   });
   var chromaticScaleDict = $Dict.fromList(_U.list([{ctor: "_Tuple2"
                                                    ,_0: "C"
                                                    ,_1: 0}
                                                   ,{ctor: "_Tuple2",_0: "C#",_1: 1}
                                                   ,{ctor: "_Tuple2",_0: "Db",_1: 1}
                                                   ,{ctor: "_Tuple2",_0: "D",_1: 2}
                                                   ,{ctor: "_Tuple2",_0: "D#",_1: 3}
                                                   ,{ctor: "_Tuple2",_0: "Eb",_1: 3}
                                                   ,{ctor: "_Tuple2",_0: "E",_1: 4}
                                                   ,{ctor: "_Tuple2",_0: "F",_1: 5}
                                                   ,{ctor: "_Tuple2",_0: "F#",_1: 6}
                                                   ,{ctor: "_Tuple2",_0: "Gb",_1: 6}
                                                   ,{ctor: "_Tuple2",_0: "G",_1: 7}
                                                   ,{ctor: "_Tuple2",_0: "G#",_1: 8}
                                                   ,{ctor: "_Tuple2",_0: "Ab",_1: 8}
                                                   ,{ctor: "_Tuple2",_0: "A",_1: 9}
                                                   ,{ctor: "_Tuple2",_0: "A#",_1: 10}
                                                   ,{ctor: "_Tuple2",_0: "Bb",_1: 10}
                                                   ,{ctor: "_Tuple2",_0: "B",_1: 11}]));
   var majorIntervals = _U.list([2,2,1,2,2,2,1]);
   var equivalentEnharmonic = function (k) {
      var _p4 = k;
      _v2_4: do {
         if (_p4.ctor === "_Tuple2" && _p4._1.ctor === "Just" && _p4._1._0.ctor === "Sharp")
         {
               switch (_p4._0.ctor)
               {case "A": return {ctor: "_Tuple2"
                                 ,_0: $Abc$ParseTree.B
                                 ,_1: $Maybe.Just($Abc$ParseTree.Flat)};
                  case "C": return {ctor: "_Tuple2"
                                   ,_0: $Abc$ParseTree.D
                                   ,_1: $Maybe.Just($Abc$ParseTree.Flat)};
                  case "D": return {ctor: "_Tuple2"
                                   ,_0: $Abc$ParseTree.E
                                   ,_1: $Maybe.Just($Abc$ParseTree.Flat)};
                  case "G": return {ctor: "_Tuple2"
                                   ,_0: $Abc$ParseTree.A
                                   ,_1: $Maybe.Just($Abc$ParseTree.Flat)};
                  default: break _v2_4;}
            } else {
               break _v2_4;
            }
      } while (false);
      return k;
   };
   var flatScale = _U.list([{ctor: "_Tuple2"
                            ,_0: $Abc$ParseTree.C
                            ,_1: $Maybe.Nothing}
                           ,{ctor: "_Tuple2"
                            ,_0: $Abc$ParseTree.D
                            ,_1: $Maybe.Just($Abc$ParseTree.Flat)}
                           ,{ctor: "_Tuple2",_0: $Abc$ParseTree.D,_1: $Maybe.Nothing}
                           ,{ctor: "_Tuple2"
                            ,_0: $Abc$ParseTree.E
                            ,_1: $Maybe.Just($Abc$ParseTree.Flat)}
                           ,{ctor: "_Tuple2",_0: $Abc$ParseTree.E,_1: $Maybe.Nothing}
                           ,{ctor: "_Tuple2",_0: $Abc$ParseTree.F,_1: $Maybe.Nothing}
                           ,{ctor: "_Tuple2"
                            ,_0: $Abc$ParseTree.G
                            ,_1: $Maybe.Just($Abc$ParseTree.Flat)}
                           ,{ctor: "_Tuple2",_0: $Abc$ParseTree.G,_1: $Maybe.Nothing}
                           ,{ctor: "_Tuple2"
                            ,_0: $Abc$ParseTree.A
                            ,_1: $Maybe.Just($Abc$ParseTree.Flat)}
                           ,{ctor: "_Tuple2",_0: $Abc$ParseTree.A,_1: $Maybe.Nothing}
                           ,{ctor: "_Tuple2"
                            ,_0: $Abc$ParseTree.B
                            ,_1: $Maybe.Just($Abc$ParseTree.Flat)}
                           ,{ctor: "_Tuple2",_0: $Abc$ParseTree.B,_1: $Maybe.Nothing}]);
   var extremeFlatScale = function () {
      var f = function (pc) {
         var _p5 = pc;
         _v3_2: do {
            if (_p5.ctor === "_Tuple2" && _p5._1.ctor === "Nothing") {
                  switch (_p5._0.ctor)
                  {case "E": return {ctor: "_Tuple2"
                                    ,_0: $Abc$ParseTree.F
                                    ,_1: $Maybe.Just($Abc$ParseTree.Flat)};
                     case "B": return {ctor: "_Tuple2"
                                      ,_0: $Abc$ParseTree.C
                                      ,_1: $Maybe.Just($Abc$ParseTree.Flat)};
                     default: break _v3_2;}
               } else {
                  break _v3_2;
               }
         } while (false);
         return pc;
      };
      return A2($List.map,f,flatScale);
   }();
   var sharpScale = _U.list([{ctor: "_Tuple2"
                             ,_0: $Abc$ParseTree.C
                             ,_1: $Maybe.Nothing}
                            ,{ctor: "_Tuple2"
                             ,_0: $Abc$ParseTree.C
                             ,_1: $Maybe.Just($Abc$ParseTree.Sharp)}
                            ,{ctor: "_Tuple2",_0: $Abc$ParseTree.D,_1: $Maybe.Nothing}
                            ,{ctor: "_Tuple2"
                             ,_0: $Abc$ParseTree.D
                             ,_1: $Maybe.Just($Abc$ParseTree.Sharp)}
                            ,{ctor: "_Tuple2",_0: $Abc$ParseTree.E,_1: $Maybe.Nothing}
                            ,{ctor: "_Tuple2",_0: $Abc$ParseTree.F,_1: $Maybe.Nothing}
                            ,{ctor: "_Tuple2"
                             ,_0: $Abc$ParseTree.F
                             ,_1: $Maybe.Just($Abc$ParseTree.Sharp)}
                            ,{ctor: "_Tuple2",_0: $Abc$ParseTree.G,_1: $Maybe.Nothing}
                            ,{ctor: "_Tuple2"
                             ,_0: $Abc$ParseTree.G
                             ,_1: $Maybe.Just($Abc$ParseTree.Sharp)}
                            ,{ctor: "_Tuple2",_0: $Abc$ParseTree.A,_1: $Maybe.Nothing}
                            ,{ctor: "_Tuple2"
                             ,_0: $Abc$ParseTree.A
                             ,_1: $Maybe.Just($Abc$ParseTree.Sharp)}
                            ,{ctor: "_Tuple2",_0: $Abc$ParseTree.B,_1: $Maybe.Nothing}]);
   var extremeSharpScale = function () {
      var f = function (pc) {
         var _p6 = pc;
         _v4_2: do {
            if (_p6.ctor === "_Tuple2" && _p6._1.ctor === "Nothing") {
                  switch (_p6._0.ctor)
                  {case "F": return {ctor: "_Tuple2"
                                    ,_0: $Abc$ParseTree.E
                                    ,_1: $Maybe.Just($Abc$ParseTree.Sharp)};
                     case "C": return {ctor: "_Tuple2"
                                      ,_0: $Abc$ParseTree.B
                                      ,_1: $Maybe.Just($Abc$ParseTree.Sharp)};
                     default: break _v4_2;}
               } else {
                  break _v4_2;
               }
         } while (false);
         return pc;
      };
      return A2($List.map,f,sharpScale);
   }();
   var majorScale = function (target) {
      var chromaticScale = _U.eq(target,
      {ctor: "_Tuple2"
      ,_0: $Abc$ParseTree.G
      ,_1: $Maybe.Just($Abc$ParseTree.Flat)}) || _U.eq(target,
      {ctor: "_Tuple2"
      ,_0: $Abc$ParseTree.C
      ,_1: $Maybe.Just($Abc$ParseTree.Flat)}) ? extremeFlatScale : isFlatMajorKey(target) ? flatScale : _U.eq(target,
      {ctor: "_Tuple2"
      ,_0: $Abc$ParseTree.F
      ,_1: $Maybe.Just($Abc$ParseTree.Sharp)}) || _U.eq(target,
      {ctor: "_Tuple2"
      ,_0: $Abc$ParseTree.C
      ,_1: $Maybe.Just($Abc$ParseTree.Sharp)}) ? extremeSharpScale : sharpScale;
      var f = lookUp(A2(rotateFrom,target,chromaticScale));
      return A2($List.map,f,partialSum(majorIntervals));
   };
   var modalScale = F2(function (target,mode) {
      var index = A2($Maybe.withDefault,
      0,
      A2($List$Extra.elemIndex,target,sharpScale));
      var distance = function () {
         var _p7 = mode;
         switch (_p7.ctor)
         {case "Minor": return 3;
            case "Dorian": return 10;
            case "Phrygian": return 8;
            case "Lydian": return 7;
            case "Mixolydian": return 5;
            case "Aeolian": return 3;
            case "Locrian": return 1;
            default: return 0;}
      }();
      var majorKeyIndex = A2($Basics._op["%"],index + distance,12);
      var majorKey = A2(lookUp,sharpScale,majorKeyIndex);
      return majorScale(equivalentEnharmonic(majorKey));
   });
   var noteDuration = F2(function (t,n) {
      return 60.0 * $Ratio.toFloat(t.unitNoteLength) / ($Ratio.toFloat(t.tempoNoteLength) * $Basics.toFloat(t.bpm)) * $Ratio.toFloat(n);
   });
   var dotFactor = function (i) {
      var _p8 = i;
      switch (_p8)
      {case 1: return 0.5;
         case 2: return 0.75;
         case 3: return 0.875;
         default: return 0;}
   };
   var modifyKeySet = F2(function (target,ks) {
      var f = function (key) {
         return !_U.eq($Basics.fst(key),target.pitchClass);
      };
      var newks = A2($List.filter,f,ks);
      return _U.eq(target.accidental,
      $Abc$ParseTree.Natural) ? ks : A2($List._op["::"],
      {ctor: "_Tuple2"
      ,_0: target.pitchClass
      ,_1: $Maybe.Just(target.accidental)},
      ks);
   });
   var accidentalInKeySet = F2(function (n,ks) {
      var f = function (_p9) {
         var _p10 = _p9;
         return {ctor: "_Tuple2"
                ,_0: $Basics.toString(_p10._0)
                ,_1: _p10._1};
      };
      var comparableks = A2($List.map,f,ks);
      var lookup = $Dict.fromList(comparableks);
      return $Maybe$Extra.join(A2($Dict.get,
      $Basics.toString(n.pitchClass),
      lookup));
   });
   var scale = function (ks) {
      var target = {ctor: "_Tuple2"
                   ,_0: ks.pitchClass
                   ,_1: ks.accidental};
      var _p11 = ks.mode;
      switch (_p11.ctor)
      {case "Major": return majorScale(target);
         case "Ionian": return majorScale(target);
         default: return A2(modalScale,target,ks.mode);}
   };
   var keySet = function (ks) {
      return A2($List.filter,accidentalKey,scale(ks));
   };
   var modifiedKeySet = function (ksm) {
      var _p12 = ksm;
      var ksig = _p12._0;
      var mods = _p12._1;
      var ks = keySet(ksig);
      return $List.isEmpty(mods) ? ks : A3($List.foldr,
      modifyKeySet,
      ks,
      mods);
   };
   var accidentalImplicitInKey = F2(function (n,mks) {
      return A2(accidentalInKeySet,n,modifiedKeySet(mks));
   });
   var midiPitchOffset = F3(function (n,mks,barAccidentals) {
      var f = function (a) {
         var _p13 = a;
         switch (_p13.ctor)
         {case "Sharp": return "#";
            case "Flat": return "b";
            default: return "";}
      };
      var inKeyAccidental = A2(accidentalImplicitInKey,n,mks);
      var inBarAccidental = A2(accidentalInKeySet,n,barAccidentals);
      var maybeAccidental = $Maybe.oneOf(_U.list([n.accidental
                                                 ,inBarAccidental
                                                 ,inKeyAccidental]));
      var accidental = A2($Maybe.withDefault,
      "",
      A2($Maybe.map,f,maybeAccidental));
      var pattern = A2($Basics._op["++"],
      $Basics.toString(n.pitchClass),
      accidental);
      return A2($Maybe.withDefault,
      0,
      A2($Dict.get,pattern,chromaticScaleDict));
   });
   var toMidiPitch = F3(function (n,mks,barAccidentals) {
      return n.octave * 12 + A3(midiPitchOffset,
      n,
      mks,
      barAccidentals);
   });
   var AbcTempo = F3(function (a,b,c) {
      return {tempoNoteLength: a,bpm: b,unitNoteLength: c};
   });
   return _elm.Music.Notation.values = {_op: _op
                                       ,keySet: keySet
                                       ,modifiedKeySet: modifiedKeySet
                                       ,scale: scale
                                       ,accidentalImplicitInKey: accidentalImplicitInKey
                                       ,dotFactor: dotFactor
                                       ,toMidiPitch: toMidiPitch
                                       ,noteDuration: noteDuration
                                       ,AbcTempo: AbcTempo};
};
Elm.Melody = Elm.Melody || {};
Elm.Melody.make = function (_elm) {
   "use strict";
   _elm.Melody = _elm.Melody || {};
   if (_elm.Melody.values) return _elm.Melody.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Abc$ParseTree = Elm.Abc.ParseTree.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Music$Notation = Elm.Music.Notation.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var ABar = F5(function (a,b,c,d,e) {
      return {number: a
             ,repeat: b
             ,iteration: c
             ,accidentals: d
             ,notes: e};
   });
   var AChord = function (a) {    return {ctor: "AChord",_0: a};};
   var ANote = F2(function (a,b) {
      return {ctor: "ANote",_0: a,_1: b};
   });
   var SingleNote = F4(function (a,b,c,d) {
      return {time: a,pitch: b,pc: c,accidental: d};
   });
   return _elm.Melody.values = {_op: _op
                               ,SingleNote: SingleNote
                               ,ABar: ABar
                               ,ANote: ANote
                               ,AChord: AChord};
};
Elm.Repeats = Elm.Repeats || {};
Elm.Repeats.make = function (_elm) {
   "use strict";
   _elm.Repeats = _elm.Repeats || {};
   if (_elm.Repeats.values) return _elm.Repeats.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Abc$ParseTree = Elm.Abc.ParseTree.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $List$Extra = Elm.List.Extra.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Maybe$Extra = Elm.Maybe.Extra.make(_elm),
   $Melody = Elm.Melody.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var slice = F2(function (start,end) {
      return function (_p0) {
         return A2($List$Extra.takeWhile,
         function (bar) {
            return _U.cmp(bar.number,end) < 0;
         },
         A2($List$Extra.dropWhile,
         function (bar) {
            return _U.cmp(bar.number,start) < 0;
         },
         _p0));
      };
   });
   var variantSlice = F5(function (start,
   firstRepeat,
   secondRepeat,
   end,
   ml) {
      var section = A3(slice,start,end,ml);
      var firstSection = A3(slice,start,secondRepeat,section);
      var secondSection = A2($Basics._op["++"],
      A3(slice,start,firstRepeat,section),
      A3(slice,secondRepeat,end,section));
      return A2($Basics._op["++"],firstSection,secondSection);
   });
   var repeatedSection = F3(function (ml,s,acc) {
      var section = function (_p1) {
         var _p2 = _p1;
         return {ctor: "_Tuple4"
                ,_0: _p2.start
                ,_1: _p2.firstEnding
                ,_2: _p2.secondEnding
                ,_3: _p2.end};
      };
      var _p3 = section(s);
      if (_p3.ctor === "_Tuple4" && _p3._0.ctor === "Just" && _p3._3.ctor === "Just")
      {
            if (_p3._1.ctor === "Just" && _p3._2.ctor === "Just") {
                  return A2($Basics._op["++"],
                  A5(variantSlice,_p3._0._0,_p3._1._0,_p3._2._0,_p3._3._0,ml),
                  acc);
               } else {
                  var _p5 = _p3._3._0;
                  var _p4 = _p3._0._0;
                  return A2($Basics._op["++"],
                  A2($Basics._op["++"],A3(slice,_p4,_p5,ml),A3(slice,_p4,_p5,ml)),
                  acc);
               }
         } else {
            return _U.list([]);
         }
   });
   var hasFirstEnding = function (s) {
      return $Maybe$Extra.isJust(s.firstEnding);
   };
   var secondRepeat = F2(function (pos,s) {
      return _U.update(s,{secondEnding: $Maybe.Just(pos)});
   });
   var firstRepeat = F2(function (pos,s) {
      return _U.update(s,{firstEnding: $Maybe.Just(pos)});
   });
   var endCurrentSection = F2(function (pos,s) {
      return _U.update(s,{end: $Maybe.Just(pos)});
   });
   var nullSection = {start: $Maybe.Just(0)
                     ,firstEnding: $Maybe.Nothing
                     ,secondEnding: $Maybe.Nothing
                     ,end: $Maybe.Just(0)};
   var isNullSection = function (s) {
      return _U.eq(s,nullSection);
   };
   var accumulateSection = function (r) {
      return $Basics.not(isNullSection(r.current)) ? _U.update(r,
      {repeats: A2($List._op["::"],r.current,r.repeats)
      ,current: nullSection}) : r;
   };
   var endAndStartSection = F3(function (endpos,startpos,r) {
      var newCurrent = _U.update(nullSection,
      {start: $Maybe.Just(startpos)});
      var current = r.current;
      var endCurrent = _U.update(current,{end: $Maybe.Just(endpos)});
      var endState = _U.update(r,{current: endCurrent});
      var newState = accumulateSection(endState);
      return _U.update(newState,{current: newCurrent});
   });
   var startSection = F2(function (pos,r) {
      return A3(endAndStartSection,pos,pos,r);
   });
   var endSection = F2(function (pos,r) {
      if (hasFirstEnding(r.current)) {
            var current = A2(endCurrentSection,pos,r.current);
            return _U.update(r,{current: current});
         } else return A3(endAndStartSection,pos,0,r);
   });
   var buildRepeatedMelody = function (_p6) {
      var _p7 = _p6;
      var _p9 = _p7._1;
      var _p8 = _p7._0;
      return $List.isEmpty(_p9) ? _p8 : A3($List.foldr,
      repeatedSection(_p8),
      _U.list([]),
      _p9);
   };
   var indexBar = F2(function (b,r) {
      var _p10 = {ctor: "_Tuple2",_0: b.iteration,_1: b.repeat};
      _v3_5: do {
         _v3_4: do {
            _v3_3: do {
               _v3_2: do {
                  _v3_1: do {
                     _v3_0: do {
                        if (_p10.ctor === "_Tuple2") {
                              if (_p10._1.ctor === "Just") {
                                    switch (_p10._1._0.ctor)
                                    {case "Begin": if (_p10._0.ctor === "Just") {
                                               switch (_p10._0._0)
                                               {case 1: break _v3_0;
                                                  case 2: break _v3_1;
                                                  default: break _v3_2;}
                                            } else {
                                               break _v3_2;
                                            }
                                       case "End": if (_p10._0.ctor === "Just") {
                                               switch (_p10._0._0)
                                               {case 1: break _v3_0;
                                                  case 2: break _v3_1;
                                                  default: break _v3_3;}
                                            } else {
                                               break _v3_3;
                                            }
                                       default: if (_p10._0.ctor === "Just") {
                                               switch (_p10._0._0)
                                               {case 1: break _v3_0;
                                                  case 2: break _v3_1;
                                                  default: break _v3_4;}
                                            } else {
                                               break _v3_4;
                                            }}
                                 } else {
                                    if (_p10._0.ctor === "Just") {
                                          switch (_p10._0._0)
                                          {case 1: break _v3_0;
                                             case 2: break _v3_1;
                                             default: break _v3_5;}
                                       } else {
                                          break _v3_5;
                                       }
                                 }
                           } else {
                              break _v3_5;
                           }
                     } while (false);
                     return _U.update(r,
                     {current: A2(firstRepeat,b.number,r.current)});
                  } while (false);
                  return _U.update(r,
                  {current: A2(secondRepeat,b.number,r.current)});
               } while (false);
               return A2(startSection,b.number,r);
            } while (false);
            return A2(endSection,b.number,r);
         } while (false);
         return A3(endAndStartSection,b.number,b.number,r);
      } while (false);
      return r;
   });
   var finalise = F2(function (lastBar,r) {
      var current = r.current;
      var newLastBar = $Maybe$Extra.isJust(current.firstEnding) ? _U.update(lastBar,
      {repeat: $Maybe.Just($Abc$ParseTree.End)}) : lastBar;
      var newr = A2(indexBar,newLastBar,r);
      return isNullSection(newr.current) ? newr : accumulateSection(newr);
   });
   var defaultRepeatState = {current: nullSection
                            ,repeats: _U.list([])};
   var RepeatState = F2(function (a,b) {
      return {current: a,repeats: b};
   });
   var Section = F4(function (a,b,c,d) {
      return {start: a,firstEnding: b,secondEnding: c,end: d};
   });
   return _elm.Repeats.values = {_op: _op
                                ,indexBar: indexBar
                                ,defaultRepeatState: defaultRepeatState
                                ,finalise: finalise
                                ,buildRepeatedMelody: buildRepeatedMelody
                                ,Section: Section
                                ,RepeatState: RepeatState};
};
Elm.AbcPerformance = Elm.AbcPerformance || {};
Elm.AbcPerformance.make = function (_elm) {
   "use strict";
   _elm.AbcPerformance = _elm.AbcPerformance || {};
   if (_elm.AbcPerformance.values)
   return _elm.AbcPerformance.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Abc = Elm.Abc.make(_elm),
   $Abc$ParseTree = Elm.Abc.ParseTree.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Melody = Elm.Melody.make(_elm),
   $Music$Notation = Elm.Music.Notation.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Repeats = Elm.Repeats.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var reverseMelody = function () {
      var reverseBar = function (b) {
         return _U.update(b,{notes: $List.reverse(b.notes)});
      };
      return function (_p0) {
         return $List.reverse(A2($List.map,reverseBar,_p0));
      };
   }();
   var translateNotePair = F4(function (n1,s1,n2,s2) {
      var barAccidentals = s1.thisBar.accidentals;
      var duration2 = A2($Music$Notation.noteDuration,
      s2.tempo,
      n2.duration) * s2.tempoModifier;
      var note2 = A2($Melody.ANote,
      {time: duration2
      ,pitch: A3($Music$Notation.toMidiPitch,
      n2,
      s2.modifiedKeySignature,
      barAccidentals)
      ,pc: $Maybe.Just(n2.pitchClass)
      ,accidental: n2.accidental},
      false);
      var duration1 = A2($Music$Notation.noteDuration,
      s1.tempo,
      n1.duration) * s1.tempoModifier;
      var note1 = A2($Melody.ANote,
      {time: duration1
      ,pitch: A3($Music$Notation.toMidiPitch,
      n1,
      s1.modifiedKeySignature,
      barAccidentals)
      ,pc: $Maybe.Just(n1.pitchClass)
      ,accidental: n1.accidental},
      false);
      return _U.list([note2,note1]);
   });
   var translateNoteSequence = F3(function (isSeq,state,notes) {
      var f = function (abc) {
         var barAccidentals = state.thisBar.accidentals;
         var duration = A2($Music$Notation.noteDuration,
         state.tempo,
         abc.duration) * state.tempoModifier;
         return {time: duration
                ,pitch: A3($Music$Notation.toMidiPitch,
                abc,
                state.modifiedKeySignature,
                barAccidentals)
                ,pc: $Maybe.Just(abc.pitchClass)
                ,accidental: abc.accidental};
      };
      return isSeq ? $List.reverse(A2($List.map,
      function (a) {
         return A2($Melody.ANote,a,false);
      },
      A2($List.map,f,notes))) : _U.list([$Melody.AChord(A2($List.map,
      f,
      notes))]);
   });
   var addNoteToBarAccidentals = F2(function (n,ks) {
      var _p1 = {ctor: "_Tuple2",_0: n.pc,_1: n.accidental};
      if (_p1.ctor === "_Tuple2" && _p1._0.ctor === "Just" && _p1._1.ctor === "Just")
      {
            var keyClass = {ctor: "_Tuple2"
                           ,_0: _p1._0._0
                           ,_1: n.accidental};
            return $Basics.not(A2($List.member,
            keyClass,
            ks)) ? A2($List._op["::"],keyClass,ks) : ks;
         } else {
            return ks;
         }
   });
   var addNoteEventToBarAccidentals = F2(function (ne,ks) {
      var _p2 = ne;
      if (_p2.ctor === "ANote") {
            return A2(addNoteToBarAccidentals,_p2._0,ks);
         } else {
            return A3($List.foldl,addNoteToBarAccidentals,ks,_p2._0);
         }
   });
   var addNoteEventsToBarAccidentals = F2(function (nes,ks) {
      return A3($List.foldl,addNoteEventToBarAccidentals,ks,nes);
   });
   var addNotesToState = F2(function (ns,state) {
      var thisBar = state.thisBar;
      var accidentals = A2(addNoteEventsToBarAccidentals,
      ns,
      thisBar.accidentals);
      var line = state.thisBar.notes;
      return _U.update(state,
      {thisBar: _U.update(thisBar,
      {notes: A2($List.append,ns,line),accidentals: accidentals})});
   });
   var addNoteToState = F2(function (n,state) {
      var thisBar = state.thisBar;
      var accidentals = A2(addNoteEventToBarAccidentals,
      n,
      thisBar.accidentals);
      var line = state.thisBar.notes;
      return _U.update(state,
      {thisBar: _U.update(thisBar,
      {notes: A2($List._op["::"],n,line),accidentals: accidentals})});
   });
   var updateState = F2(function (h,acc) {
      var _p3 = acc;
      var melody = _p3._0;
      var state = _p3._1;
      var tempo = state.tempo;
      var _p4 = h;
      switch (_p4.ctor)
      {case "UnitNoteLength": return {ctor: "_Tuple2"
                                     ,_0: melody
                                     ,_1: _U.update(state,
                                     {tempo: _U.update(tempo,{unitNoteLength: _p4._0})})};
         case "Tempo": var _p5 = _p4._0;
           var tnl = A3($List.foldl,
           $Ratio.add,
           $Ratio.fromInt(0),
           _p5.noteLengths);
           return {ctor: "_Tuple2"
                  ,_0: melody
                  ,_1: _U.update(state,
                  {tempo: _U.update(tempo,{tempoNoteLength: tnl,bpm: _p5.bpm})})};
         case "Key": return {ctor: "_Tuple2"
                            ,_0: melody
                            ,_1: _U.update(state,{modifiedKeySignature: _p4._0})};
         default: return acc;}
   });
   var isEmptyBar = function (b) {
      return _U.eq($List.length(b.notes),0);
   };
   var defaultBar = function (i) {
      return {number: i
             ,repeat: $Maybe.Nothing
             ,iteration: $Maybe.Nothing
             ,accidentals: _U.list([])
             ,notes: _U.list([])};
   };
   var buildNewBar = F3(function (nextBarNumber,abcBar,lastBar) {
      var nextBar = defaultBar(nextBarNumber);
      if (isEmptyBar(lastBar)) {
            var _p6 = {ctor: "_Tuple2"
                      ,_0: lastBar.repeat
                      ,_1: abcBar.repeat};
            if (_p6.ctor === "_Tuple2" && _p6._0.ctor === "Just") {
                  if (_p6._0._0.ctor === "End" && _p6._1.ctor === "Just" && _p6._1._0.ctor === "Begin")
                  {
                        return A2($Debug.log,
                        "Just End, Just Begin",
                        _U.update(nextBar,
                        {repeat: $Maybe.Just($Abc$ParseTree.BeginAndEnd)
                        ,iteration: abcBar.iteration}));
                     } else {
                        return A2($Debug.log,
                        "Just x",
                        _U.update(nextBar,
                        {repeat: $Maybe.Just(_p6._0._0),iteration: abcBar.iteration}));
                     }
               } else {
                  return _U.update(nextBar,
                  {repeat: abcBar.repeat,iteration: abcBar.iteration});
               }
         } else return _U.update(nextBar,
         {repeat: abcBar.repeat,iteration: abcBar.iteration});
   });
   var translateMusic = F2(function (m,acc) {
      var _p7 = acc;
      var melodyLine = _p7._0;
      var state = _p7._1;
      var _p8 = m;
      switch (_p8.ctor)
      {case "Note": var _p9 = _p8._0;
           var barAccidentals = state.thisBar.accidentals;
           var duration = A2($Music$Notation.noteDuration,
           state.tempo,
           _p9.duration) * state.tempoModifier;
           var note = A2($Melody.ANote,
           {time: duration
           ,pitch: A3($Music$Notation.toMidiPitch,
           _p9,
           state.modifiedKeySignature,
           barAccidentals)
           ,pc: $Maybe.Just(_p9.pitchClass)
           ,accidental: _p9.accidental},
           _p9.tied);
           var newState = A2(addNoteToState,note,state);
           return {ctor: "_Tuple2",_0: melodyLine,_1: newState};
         case "Rest": var duration = A2($Music$Notation.noteDuration,
           state.tempo,
           _p8._0) * state.tempoModifier;
           var note = A2($Melody.ANote,
           {time: duration
           ,pitch: 0
           ,pc: $Maybe.Nothing
           ,accidental: $Maybe.Nothing},
           false);
           var newState = A2(addNoteToState,note,state);
           return {ctor: "_Tuple2",_0: melodyLine,_1: newState};
         case "Tuplet": var _p10 = _p8._0;
           var p = _p10._0;
           var q = _p10._1;
           var r = _p10._2;
           var tupletState = _U.update(state,
           {tempoModifier: $Basics.toFloat(q) / $Basics.toFloat(p)});
           var tupletNotes = A3(translateNoteSequence,
           true,
           tupletState,
           _p8._1);
           var newState = A2(addNotesToState,tupletNotes,state);
           return {ctor: "_Tuple2",_0: melodyLine,_1: newState};
         case "BrokenRhythmPair": var _p15 = _p8._2;
           var _p14 = _p8._0;
           var _p11 = _p8._1;
           if (_p11.ctor === "LeftArrow") {
                 var _p12 = _p11._0;
                 var rightState = _U.update(state,
                 {tempoModifier: 1 + $Music$Notation.dotFactor(_p12)});
                 var leftState = _U.update(state,
                 {tempoModifier: 1 - $Music$Notation.dotFactor(_p12)});
                 var notePair = A4(translateNotePair,
                 _p14,
                 leftState,
                 _p15,
                 rightState);
                 var newState = A2(addNotesToState,notePair,state);
                 return {ctor: "_Tuple2",_0: melodyLine,_1: newState};
              } else {
                 var _p13 = _p11._0;
                 var rightState = _U.update(state,
                 {tempoModifier: 1 - $Music$Notation.dotFactor(_p13)});
                 var leftState = _U.update(state,
                 {tempoModifier: 1 + $Music$Notation.dotFactor(_p13)});
                 var notePair = A4(translateNotePair,
                 _p14,
                 leftState,
                 _p15,
                 rightState);
                 var newState = A2(addNotesToState,notePair,state);
                 return {ctor: "_Tuple2",_0: melodyLine,_1: newState};
              }
         case "Chord": var chord = A3(translateNoteSequence,
           false,
           state,
           _p8._0.notes);
           var newState = A2(addNotesToState,chord,state);
           return {ctor: "_Tuple2",_0: melodyLine,_1: newState};
         case "Barline":
         var repeatState = isEmptyBar(state.thisBar) ? state.repeatState : A2($Repeats.indexBar,
           state.thisBar,
           state.repeatState);
           var nextBarNumber = isEmptyBar(state.thisBar) ? state.nextBarNumber : state.nextBarNumber + 1;
           var newBar = A3(buildNewBar,nextBarNumber,_p8._0,state.thisBar);
           var newState = _U.update(state,
           {thisBar: newBar
           ,nextBarNumber: nextBarNumber
           ,repeatState: repeatState});
           var newMelody = function () {
              if (isEmptyBar(state.thisBar)) return melodyLine; else {
                    var rb = A2($Debug.log,"the next bar",state.thisBar);
                    return A2($List._op["::"],state.thisBar,melodyLine);
                 }
           }();
           return {ctor: "_Tuple2",_0: newMelody,_1: newState};
         default: return acc;}
   });
   var toMelodyLine = F2(function (ml,state) {
      var _p16 = A3($List.foldl,translateMusic,state,ml);
      var melody = _p16._0;
      var s = _p16._1;
      return {ctor: "_Tuple2",_0: melody,_1: s};
   });
   var defaultKey = {ctor: "_Tuple2"
                    ,_0: {pitchClass: $Abc$ParseTree.C
                         ,accidental: $Maybe.Nothing
                         ,mode: $Abc$ParseTree.Major}
                    ,_1: _U.list([])};
   var defaultTempo = {tempoNoteLength: A2($Ratio.over,1,4)
                      ,bpm: 120
                      ,unitNoteLength: A2($Ratio.over,1,8)};
   var fromAbc = function (tune) {
      var f = F2(function (bp,acc) {
         var _p17 = bp;
         if (_p17.ctor === "Score") {
               var _p18 = A2(toMelodyLine,_p17._0,acc);
               var newLine = _p18._0;
               var newState = _p18._1;
               var _p19 = acc;
               var existingLine = _p19._0;
               var state = _p19._1;
               return {ctor: "_Tuple2",_0: newLine,_1: newState};
            } else {
               return A2(updateState,_p17._0,acc);
            }
      });
      var defaultState = {ctor: "_Tuple2"
                         ,_0: _U.list([])
                         ,_1: {modifiedKeySignature: defaultKey
                              ,tempo: defaultTempo
                              ,tempoModifier: 1.0
                              ,nextBarNumber: 0
                              ,thisBar: defaultBar(0)
                              ,repeatState: $Repeats.defaultRepeatState}};
      var headerState = A3($List.foldl,
      updateState,
      defaultState,
      $Basics.fst(tune));
      var _p20 = A3($List.foldl,f,headerState,$Basics.snd(tune));
      var music = _p20._0;
      var state = _p20._1;
      var fullMusic = isEmptyBar(state.thisBar) ? reverseMelody(music) : reverseMelody(A2($List._op["::"],
      state.thisBar,
      music));
      var repeatState = A2($Repeats.finalise,
      A2($Debug.log,"last bar",state.thisBar),
      A2($Debug.log,"repeat state",state.repeatState));
      return {ctor: "_Tuple2"
             ,_0: fullMusic
             ,_1: $List.reverse(repeatState.repeats)};
   };
   var melodyFromAbc = F2(function (expandRepeats,tune) {
      var mr = fromAbc(tune);
      return expandRepeats ? {ctor: "_Tuple2"
                             ,_0: $Repeats.buildRepeatedMelody(mr)
                             ,_1: _U.list([])} : mr;
   });
   var fromAbcResult = function (r) {
      return A2($Result.map,fromAbc,r);
   };
   var melodyFromAbcResult = function (r) {
      return A2($Result.map,
      function (_p21) {
         return $Repeats.buildRepeatedMelody(fromAbc(_p21));
      },
      r);
   };
   var TranslationState = F6(function (a,b,c,d,e,f) {
      return {modifiedKeySignature: a
             ,tempo: b
             ,tempoModifier: c
             ,nextBarNumber: d
             ,thisBar: e
             ,repeatState: f};
   });
   return _elm.AbcPerformance.values = {_op: _op
                                       ,fromAbc: fromAbc
                                       ,fromAbcResult: fromAbcResult
                                       ,melodyFromAbc: melodyFromAbc
                                       ,melodyFromAbcResult: melodyFromAbcResult};
};
Elm.Native.SoundFont = {};
Elm.Native.SoundFont.make = function (localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.SoundFont = localRuntime.Native.SoundFont || {};

    if (Elm.Native.SoundFont.values) return Elm.Native.SoundFont.values;	
   
    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);      
    var Maybe = Elm.Maybe.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);     

    var values = {};
    values.soundfontSignal = NS.constant(Maybe.Nothing);

    values.context = new (window.AudioContext || window.webkitAudioContext)();

   
    /* Get the current time from the audio context */
    values.getCurrentTime = function() {
      return values.context.currentTime;
    };


    /*
     * nameToUrl
     * Given an instrument name returns a URL to its Soundfont js file
     * (we only use acoustic grand piano at the moment)
     *
     * @param {String} name - instrument name
     * @returns {String} the Soundfont data url
     */
     values.nameToUrl = function(name) {
       return 'assets/soundfonts/' + name + '-ogg.js';
     }

    /*
     * SoundFont.getScript
     *
     * Given a script URL returns a Promise with the script contents as text
     * @param {String} url - the URL
     */
    values.loadData = function(url) {
      return new Promise(function(done, reject) {
        var req = new XMLHttpRequest();
        req.open('GET', url);

        req.onload = function() {
          if (req.status == 200) {
            done(req.response);
          } else {
            reject(Error(req.statusText));
          }
        };
        req.onerror = function() {
          reject(Error("Network Error"));
        };
        req.send();
      });
    }

    /*
     *  Parse the SoundFont data and return a JSON object
     *  (SoundFont data are .js files wrapping json data)
     *
     * @param {String} data - the SoundFont js file content
     * @returns {JSON} the parsed data as JSON object
     */
    values.dataToJson = function(data) {
      console.log("dataToJson");
      var begin = data.indexOf("MIDI.Soundfont.");
      begin = data.indexOf('=', begin) + 2;
      var end = data.lastIndexOf(',');
      return JSON.parse(data.slice(begin, end) + "}");
    }

    /*
     * @param {String} name - The bank name
     * @param {Object} data - The Soundfont instrument data as JSON
     */
    function createBank(name, data) {
      console.log("createBank");
      var bank = { ctx: values.context, name: name, data: data };
      bank.buffers = {};

      return bank;
    }


   /*
    * INTENAL: decodeBank
    * Given a soundfont bank, returns a Promise that resolves when
    * all the notes from the instrument are decoded
    */
   function decodeBank(bank) {
      var promises = Object.keys(bank.data).map(function(note) {
        return decodeNote(bank.ctx, bank.data[note])
         .then(function(buffer) {          
           note = parseNote(note); 
           noteName = (note.midi).toString();
           console.log("decodeBank note: ", note.name);
           localRuntime.notify(values.soundfontSignal.id, Maybe.Just(values.createAudioBuffer(noteName, buffer)));
        });
      });

      return Promise.all(promises).then(function() { 
        localRuntime.notify(values.soundfontSignal.id, Maybe.Just(values.createAudioBuffer("end", null)));
        console.log("returning after all decodeBank promises");
      })
    } 

    /*
     * Given a WAA context and a base64 encoded buffer data returns
     * a Promise that resolves when the buffer is decoded
     */
    function decodeNote(context, data) {
      return new Promise(function(done, reject) {
        var decodedData = base64DecodeToArray(data.split(",")[1]).buffer;
        context.decodeAudioData(decodedData, function(buffer) {
          done(buffer);
        }, function(e) {
          reject("DecodeAudioData error", e);
        });
      });
    }

    /* Create an AudioBuffer named after the note it provides */
    values.createAudioBuffer = function(name, buffer) {
      return {ctor: "AudioBuffer", "name" : name, "buffer" : buffer};
    };


    /*
     * loadSoundFonts
     *
     * Given an implicit Web Audio context and a instrument name
     * load the instrument data and generate a signal of sound sample audio buffers
     *
     * @param {String} name - the soundfont instrument name
     */
 
    values.loadSoundFont = function(name) {
      var promise = Promise.resolve(name)
        .then(values.nameToUrl)
        .then(values.loadData)
        .then(values.dataToJson)
        .then(function(jsonData) {
          return createBank(name, jsonData)
        })
        .then(decodeBank); 
       return values.soundfontSignal;
    }

 
    function b64ToUint6 (nChr) {
      return nChr > 64 && nChr < 91 ?
          nChr - 65
        : nChr > 96 && nChr < 123 ?
          nChr - 71
        : nChr > 47 && nChr < 58 ?
          nChr + 4
        : nChr === 43 ?
          62
        : nChr === 47 ?
          63
        :
          0;

    }

    // Decode Base64 to Uint8Array
    // ---------------------------
    function base64DecodeToArray(sBase64, nBlocksSize) {
      var sB64Enc = sBase64.replace(/[^A-Za-z0-9\+\/]/g, "");
      var nInLen = sB64Enc.length;
      var nOutLen = nBlocksSize ?
        Math.ceil((nInLen * 3 + 1 >> 2) / nBlocksSize) * nBlocksSize :
        nInLen * 3 + 1 >> 2;
      var taBytes = new Uint8Array(nOutLen);

      for (var nMod3, nMod4, nUint24 = 0, nOutIdx = 0, nInIdx = 0; nInIdx < nInLen; nInIdx++) {
        nMod4 = nInIdx & 3;
        nUint24 |= b64ToUint6(sB64Enc.charCodeAt(nInIdx)) << 18 - 6 * nMod4;
        if (nMod4 === 3 || nInLen - nInIdx === 1) {
          for (nMod3 = 0; nMod3 < 3 && nOutIdx < nOutLen; nMod3++, nOutIdx++) {
            taBytes[nOutIdx] = nUint24 >>> (16 >>> nMod3 & 24) & 255;
          }
          nUint24 = 0;
        }
      }
      return taBytes;
    }

/* PARSE NOTE */

    var NOTE = /^([a-gA-G])(#{0,2}|b{0,2})(-?[0-9]{1}|[+]{0,2}|[-]{0,2})$/
    /*
     * parseNote
     *
     * @param {String} note - the note string to be parsed
     * @return {Object} a object with the following attributes:
     * - pc: pitchClass, the letter of the note, ALWAYS in lower case
     * - acc: the accidentals (or '' if no accidentals)
     * - oct: the octave as integer. By default is 4
     */
    var parseNote = function (note, defaultOctave, defaultValue) {
      var parsed, match, octave

      // in scientific notation middleC is 4
      defaultOctave = defaultOctave || 4
      // test string against regex
      if (typeof note === 'string' && (match = NOTE.exec(note))) {
        // match[3] is the octave part
        if (match[3].length > 0 && !isNaN(match[3])) {
          octave = +match[3]
        } else if (match[3][0] === '+') {
          octave = defaultOctave + match[3].length
        } else if (match[3][0] === '-') {
          octave = defaultOctave - match[3].length
        } else {
          octave = defaultOctave
        }
        parsed = { pc: match[1].toLowerCase(),
          acc: match[2], oct: octave }
      } else if (typeof note.pc !== 'undefined'
        && typeof note.acc !== 'undefined'
        && typeof note.oct !== 'undefined') {
        parsed = note
      }

      if (parsed) {
        parsed.name = parsed.name || '' + parsed.pc + parsed.acc + parsed.oct
        parsed.midi = parsed.midi || toMidi(parsed)
        parsed.freq = parsed.freq || midiToFrequency(parsed.midi)
        return parsed
      } else if (typeof (defaultValue) !== 'undefined') {
        return defaultValue
      } else {
        throw Error('Invalid note format: ' + note)
      }
    }

    var SEMITONES = {c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 }

    function toMidi (note) {
      var alter = note.acc.length
      if (note.acc[0] === 'b') alter = -1 * alter
      return SEMITONES[note.pc] + alter + 12 * (note.oct + 1) 
    }

    function midiToFrequency (note) {
      return Math.pow(2, (note - 69) / 12) * 440
    }  

/* END OF PARSE NOTE */
 
    /* play an audio buffer at the supplied time offet and with appropriate volume (gain) */
    values.play = F3(function (buffer, time, gain) {
        console.log("buffer to play: " + buffer + " time: " + time + " with gain: " + gain)
            return Task.asyncFunction(function (callback) {
                playSound(buffer, time, gain)
                callback(Task.succeed(Utils.Tuple0));
            });
        });

    function playSound(buffer, time, gain) { 
      // console.log("playing buffer at time: " + time + " with gain: " + gain)
      var source = values.context.createBufferSource(); 
      var gainNode = values.context.createGain();
      gainNode.gain.value = gain;
      source.buffer = buffer;
      source.connect(gainNode);
      gainNode.connect(values.context.destination)
      source.start(time);
    }


    return Elm.Native.SoundFont.values = values;       
};

Elm.SoundFont = Elm.SoundFont || {};
Elm.SoundFont.make = function (_elm) {
   "use strict";
   _elm.SoundFont = _elm.SoundFont || {};
   if (_elm.SoundFont.values) return _elm.SoundFont.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$SoundFont = Elm.Native.SoundFont.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var getCurrentTime = $Native$SoundFont.getCurrentTime;
   var maybePlay = function (sb) {
      var _p0 = sb.mss;
      if (_p0.ctor === "Nothing") {
            return $Task.succeed({ctor: "_Tuple0"});
         } else {
            return A3($Native$SoundFont.play,
            _p0._0.buffer,
            getCurrentTime({ctor: "_Tuple0"}) + sb.time,
            sb.gain);
         }
   };
   var loadSoundFont = function (name) {
      return $Native$SoundFont.loadSoundFont(name);
   };
   var SoundBite = F3(function (a,b,c) {
      return {mss: a,time: b,gain: c};
   });
   var SoundSample = F2(function (a,b) {
      return {name: a,buffer: b};
   });
   var AudioBuffer = {ctor: "AudioBuffer"};
   return _elm.SoundFont.values = {_op: _op
                                  ,loadSoundFont: loadSoundFont
                                  ,getCurrentTime: getCurrentTime
                                  ,maybePlay: maybePlay
                                  ,SoundSample: SoundSample
                                  ,SoundBite: SoundBite};
};
Elm.Notable = Elm.Notable || {};
Elm.Notable.make = function (_elm) {
   "use strict";
   _elm.Notable = _elm.Notable || {};
   if (_elm.Notable.values) return _elm.Notable.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Melody = Elm.Melody.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var defaultGain = 1.0;
   var Note = F2(function (a,b) {
      return {ctor: "Note",_0: a,_1: b};
   });
   var fromNote = F3(function (n,tied,acc) {
      var _p0 = acc;
      var t = _p0._0;
      var mtie = _p0._1;
      var p = _p0._2;
      var event = {ctor: "_Tuple2"
                  ,_0: t
                  ,_1: A2(Note,n.pitch,defaultGain)};
      var nextTie = tied ? $Maybe.Just(event) : $Maybe.Nothing;
      if (_U.eq(n.pitch,0)) return {ctor: "_Tuple3"
                                   ,_0: t + n.time
                                   ,_1: $Maybe.Nothing
                                   ,_2: p}; else {
            var _p1 = mtie;
            if (_p1.ctor === "Nothing") {
                  return {ctor: "_Tuple3"
                         ,_0: t + n.time
                         ,_1: nextTie
                         ,_2: A2($List._op["::"],event,p)};
               } else {
                  return _U.eq(n.pitch,_p1._0._1._0) ? {ctor: "_Tuple3"
                                                       ,_0: t + n.time
                                                       ,_1: nextTie
                                                       ,_2: p} : {ctor: "_Tuple3"
                                                                 ,_0: t + n.time
                                                                 ,_1: nextTie
                                                                 ,_2: A2($List._op["::"],event,p)};
               }
         }
   });
   var fromChord = F2(function (ns,acc) {
      var _p2 = acc;
      var t = _p2._0;
      var mtie = _p2._1;
      var p = _p2._2;
      var noteTime = A2($Maybe.withDefault,
      0.0,
      A2($Maybe.map,
      function (n) {
         return n.time;
      },
      $List.head(ns)));
      var f = function (n) {
         return {ctor: "_Tuple2"
                ,_0: t
                ,_1: A2(Note,n.pitch,defaultGain)};
      };
      var notes = A2($List.map,f,ns);
      return {ctor: "_Tuple3"
             ,_0: t + noteTime
             ,_1: $Maybe.Nothing
             ,_2: A2($List.append,notes,p)};
   });
   var fromBar = F2(function (b,acc) {
      var f = F2(function (ne,acc) {
         var _p3 = ne;
         if (_p3.ctor === "ANote") {
               return A3(fromNote,A2($Debug.log,"note",_p3._0),_p3._1,acc);
            } else {
               return A2(fromChord,_p3._0,acc);
            }
      });
      var _p4 = acc;
      var t = _p4._0;
      var mtie = _p4._1;
      var p = _p4._2;
      return A3($List.foldl,f,acc,b.notes);
   });
   var fromMelodyLine = F2(function (t,m) {
      var _p5 = A3($List.foldl,
      fromBar,
      {ctor: "_Tuple3",_0: 0.0,_1: $Maybe.Nothing,_2: _U.list([])},
      m);
      var t = _p5._0;
      var mtie = _p5._1;
      var p = _p5._2;
      return p;
   });
   return _elm.Notable.values = {_op: _op
                                ,fromMelodyLine: fromMelodyLine
                                ,Note: Note};
};
Elm.Lessons = Elm.Lessons || {};
Elm.Lessons.make = function (_elm) {
   "use strict";
   _elm.Lessons = _elm.Lessons || {};
   if (_elm.Lessons.values) return _elm.Lessons.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var xmplBalkan = A2($Basics._op["++"],
   "X: 1\r\nT: Acano mlada nevesto\r\nO: Macedonia\r\nS: R.B.Iverson\r\nM: 11/16\r\nL: 1/16\r\nK: AMin^G\r\n",
   A2($Basics._op["++"],
   "|: E3  e2e2 d2c2 | ~B2A ~A2GA B2-B2 :: A2A dccB BAAG |\r\n",
   A2($Basics._op["++"],
   "| G2F ~F2EF ~G2FE | A2A dccB BAAG | ~G2F ~FGFE E2E2 :|\r\n",
   A2($Basics._op["++"],
   "|: EFD EFGA BcBA | Bcd cBcA BEeE |\r\n",
   "|  EFD EFGA BcBA | GAB AGFE E2-E2 :|\r\n"))));
   var instBalkan = A2($Basics._op["++"],
   "Balkan music also tends to have unusual modes and time signatures.  This tune is in A Minor with a sharpened G; the meter is 11/16.",
   " The \'~\' symbol indicates a particular decoration - a roll - but this player does not attempt it.");
   var xmplKlezmer = A2($Basics._op["++"],
   "X: 1\r\nT: Der Badchen Freylach \r\nM: 2/4\r\nL: 1/16\r\nK: Ddor^G\r\n",
   A2($Basics._op["++"],
   "|: DA,DF GAGF | A2A2 FED2 | DA,DF GAGF | A4 A4- |\r\n",
   A2($Basics._op["++"],
   "| AA,DF GAFD | A2A2 FED2 | EFGF EDEF | D8 :|\r\n",
   A2($Basics._op["++"],
   "|: ABcB dcBA | GABc A4 | dcBA GABc | A4 A4 |\r\n",
   A2($Basics._op["++"],
   "| ABcB dcBA | GABc A4 |1 ABcB AB (3FED | EFD2- D4 |\r\n",
   ":|2 GABA GAFE | D8 :||\r\n")))));
   var instKlezmer = A2($Basics._op["++"],
   "Klezmer tends to use modes that are not diatonic scales - some intervals are more than two semitones.",
   A2($Basics._op["++"],
   " Suppose you have a tune that would be in a \'standard\' mode except that one note in the scale is sharpened.",
   A2($Basics._op["++"],
   " You can either use the name of the mode in the key signature and then explicitly sharpen this note each time it occurs in the score",
   A2($Basics._op["++"],
   " or you can modify the key signature itself, adding as many (sharpened or flattened) accidentals as are needed.",
   " The following tune is in D Dorian, but with every G sharpened."))));
   var xmplMixolydian = A2($Basics._op["++"],
   "X: 1\r\nT: The Yellow Wattle\r\nR: jig\r\nM: 6/8\r\nL: 1/8\r\nK: Dmix\r\n",
   A2($Basics._op["++"],
   "|:dcA AGE|ABA ABc|dcA ABc|dcA AGE|\r\n",
   A2($Basics._op["++"],
   "dcA AGE|ABA AGE|EDD cde|dcA GED:|\r\n",
   A2($Basics._op["++"],
   "|:DED c3|ded c3|DED cde|dcA GED|\r\n",
   "DED c3|ded d2c|ABA ABc|dcA GED:|\r\n"))));
   var instMixolydian = A2($Basics._op["++"],
   "If you come across a modal tune, rather than marking its key signature as straightforward major or minor,",
   A2($Basics._op["++"],
   " you can instead use the mode name. For example, the following tune is in D Mixolydian.  But remember, the classical",
   A2($Basics._op["++"],
   " modes all use the standard diatonic scale - they just start at different places along the scale. So for this tune ",
   " the printed score would look, to all intents and purposes, identical to that for E Minor. Feel free to use either signature.")));
   var xmplChangeKey = A2($Basics._op["++"],
   "T:Polska från Småland \r\nM:3/4\r\nL:1/16\r\nR:polska\r\nK:Bmin\r\n",
   A2($Basics._op["++"],
   "|: B4 A4 B4 | d2f2 e2dc c2d2 | B2B2 A2A2 B2B2 |d2f2 e2dc d4 |\r\n",
   A2($Basics._op["++"],
   "F2GA B2AB c2Bc |d2cd edcB A2F2 | F2GA B2AB c2Bc |d2cd edcB A2F2 |\r\n",
   A2($Basics._op["++"],
   "F2GA B2c2 d3B | B2A2 B8 :|\r\n",
   A2($Basics._op["++"],
   "K:F#Min\r\n",
   A2($Basics._op["++"],
   "|: f4 e4 f4 |g2a2 b2ag g2a2 |f2f2 e2e2 f2f2 |g2a2 b2ag a4 |\r\n",
   A2($Basics._op["++"],
   "c2de f2ef g2fg |a2ga bagf e2c2 | c2de f2ef g2fg |a2ga bagf e2c2 |\r\n",
   "c2de f2g2 a3f |f2e2 f8 :|\r\n")))))));
   var instChangeKey = A2($Basics._op["++"],
   "If a tune changes key, you can indicate this simply by placing the K (key) header inside the score at the point where the key changes.",
   A2($Basics._op["++"],
   " In this example, the first part of the tune is in B Minor and the second part in F# Minor.",
   " Various other headers can be used within the score in this way - in particular, the M (meter) and L (unit note length) headers."));
   var xmplInformation = A2($Basics._op["++"],
   "X: 1\r\nT: Gubbdansen\r\nS: from 12 låtar för 2 eller 3 fioler med Gärdebylåten i Hjort Anders Olssons originalsättning\r\n",
   A2($Basics._op["++"],
   "Z: John Batchellor\r\nR: polska\r\nM: 3/4\r\nL: 1/16\r\nK:Dmin\r\n",
   A2($Basics._op["++"],
   "|: f3g f4 a4 | a2ba g2ag f2e2 | d3e f2g2 a2f2 | f3e e2^c2 A4 :|\r\n",
   A2($Basics._op["++"],
   "|: ^c2c2 d2d2 e2e2 | f2f2 gfed e4 | ^c2c2 d2d2 e2e2 | f2f2 gfed e4 |\r\n",
   "a4 b2a2 g2f2 | f2ef g2f2 e2d2 | fed^c c4 d4 :|\r\n"))));
   var instInformation = A2($Basics._op["++"],
   "There are various other headers that you can use to add information about the tune as free text.  The most important are these: ",
   " C (composer), O (geographical origin), S (source - where or how the tune was collected) and Z (the tune transcriber).");
   var xmplRhythm = A2($Basics._op["++"],
   "X: 1\r\nT: Kapten Lindholms Engelska\r\nR: engelska\r\nM: 4/4\r\nL: 1/8\r\nK:Amaj\r\n",
   A2($Basics._op["++"],
   "|: ed | cAce dcdf | ecAF E2 ed | cABc defg | aece agfe | cAce dcdf | ecAF E2 ed | cABc defg | a2 ag a2 :|\r\n",
   "|: e2 | aac\'e aac\'e | bbd\'e bbd\'e | aac\'e aac\'e | efed cB A2| fdfa ecea | fdfa ecea |fdfa gegb | baag a2 :|\r\n"));
   var instRhythm = A2($Basics._op["++"],
   "You can use the R (rhythm) header to indicate the type of tune (jig, reel, polska etc.). In most ABC collections, this field is optional.",
   A2($Basics._op["++"],
   " However, if you want to save your tune to tradtunedb, it requires a rhythm header to be present so that you can search",
   " easily for tunes of the same type"));
   var xmplTitle = "X:1\r\nT:Camptown Races\r\nM:4/4\r\nL:1/8\r\nK:D\r\n|AAFA|BAF2|FE2z|FE2z|AAFA|BAF2|E2FE|D2-D2|\r\n|D>DFA|d4|B>BdB|A3F|\r\nAA F/2F/2 A/2A/2|BAF2|EF/2-G/2FE|D4 |\r\n";
   var instTitle = A2($Basics._op["++"],
   "Very many of our previous examples have had no headers - only the melody line.  But, in fact a legitimate ABC tune always",
   A2($Basics._op["++"],
   " requires some headers.  The first is largely irrelevant - a reference number denoted by X.  Any number will do",
   A2($Basics._op["++"],
   " in most cases. The second header must be the tune title - T. You should also include the L (note length) and  M (meter) headers",
   " introduced earlier. Finally, the K (key) header should always be the last one.")));
   var xmplRepeatVariants = A2($Basics._op["++"],
   "L: 1/16\r\nK:Dmaj\r\n|: A4 a4 a2f2 | gfga b3a g2f2 | e3f g2b2 a2g2 | f3e d2c2 d2B2 |\r\n",
   "|1 B2A^G A8 :|2 B2AG F2EF A2A,2 | A,2D2 D8 |");
   var instRepeatVariants = "In some tunes, the two repeats may differ in their endings.  You can indicate that using |1 and |2 for the two variant endings";
   var xmplRepeat = "| C2 D2 E2 C2 :|: E2 F2 G4 :|\r\n|: GAGF E2 C2 :|: C2 G,2 C4 :|";
   var instRepeat = A2($Basics._op["++"],
   "You can indicate that a section should be repeated by sandwiching it between bars which use the colon as a repeat marker - |: and :|",
   " The initial repeat marker at the first bar is optional.");
   var xmplQuadruplet = "K:Amaj\r\n| (3efg a2 a>b | (3agf e2-e>e | (4f2d2e2c2 | d>f (3f2e2c2 |";
   var instQuadruplet = A2($Basics._op["++"],
   "Quadruplets are used if you want to play four notes in the time usually taken by three.",
   A2($Basics._op["++"],
   " In a similar fashion to triplets, introduce four notes of the same length placed together",
   " with the symbol (4. This example contains triplets, a tie and a quadruplet."));
   var xmplComplexTriplet = "K:Gmaj\r\n| D2 G>A B>c| (3:2:4d2d2Bd g2|";
   var instComplexTriplet = A2($Basics._op["++"],
   "If your triplet has notes of different lengths, you have to use the complex triplet notation.",
   A2($Basics._op["++"],
   " For example (3:2:4d2d2Bd means play the rhythm of three notes in the time of two over the following group",
   " of four notes."));
   var xmplTriplet = "K:Dmaj\r\n| A2 d2 e>f | (3g2f2d2 B2- |";
   var instTriplet = A2($Basics._op["++"],
   "A triplet is usually used if you want to play three notes in the time normally taken by two.",
   A2($Basics._op["++"],
   " You introduce three notes of the same length placed together with the symbol (3",
   " This is extremely common in Swedish polskas - for example the start of the Grind Hans Jässpôdspolska."));
   var xmplTie = "| G2 | c2c2 A2Ac | B2B2- B2AB |";
   var instTie = A2($Basics._op["++"],
   "A tie joins together two notes of the same pitch.  It is indicated by placing a hyphen directly after the first note of the pair.",
   A2($Basics._op["++"],
   " The second note may follow immediately, but it can be separated by spaces or even a bar line.  The effect is to play one long note",
   " with the combined duration of the pair.  If the notes are of different pitches, the tie will simply be ignored."));
   var xmplMeter = A2($Basics._op["++"],
   "X:1\r\nT:Another jig will do\r\nQ:3/8=120\r\nM:9/8\r\nK:D\r\n",
   A2($Basics._op["++"],
   "ABA A2G F2G | ABA AGF G2E |\r\n",
   A2($Basics._op["++"],
   "ABA A2G F2G | A2d d2c d3 |\r\n",
   A2($Basics._op["++"],
   "A2g f2d e2c | A2B =c2B c2B |\r\n",
   A2($Basics._op["++"],
   "A2g f2d e2^c | A2d d2c d3 |\r\n",
   A2($Basics._op["++"],
   "A2g f2d e2c | A2B =c2B c2^c |\r\n",
   "d2A A2G F2G | A2d d2c d3 |\r\n"))))));
   var instMeter = A2($Basics._op["++"],
   "The meter is defined with the M header.  For example, a waltz would normally have the meter 3/4 and a march 4/4.",
   A2($Basics._op["++"],
   " 3/4 means that each complete bar should have a total duration equal to that of three quarter notes.",
   A2($Basics._op["++"],
   " The presence of a meter actually makes little difference to how the tune sounds, but will show up in a score.",
   A2($Basics._op["++"],
   " But it is important to make sure that the duration of each complete bar agrees with the meter you designate.",
   " This example is a slip-jig in 9/8"))));
   var xmplTempo = "L: 1/8 \r\nQ: 1/4=60\r\nA B c def";
   var instTempo = A2($Basics._op["++"],
   "An accurate tempo is defined by means of the Q (tempo) header.  Up till now, we\'ve used a default where we have 120 quarter notes per minute",
   A2($Basics._op["++"],
   " i.e 1/4=120.  We can, for example, slow down our tune firstly by reverting to a unit note length of 1/8 and secondly by explicitly reducing the ",
   " tempo with the Q header."));
   var xmplUnitNote = "L: 1/16 \r\nA B c def";
   var instUnitNote = A2($Basics._op["++"],
   "You may have noticed when we first introduced notes that we talked about their duration in \'units\'.  But how long is a unit?",
   A2($Basics._op["++"],
   " So far, we have used the convention that it represents an eighth note (a quaver).  In other words, in a score, this is how",
   A2($Basics._op["++"],
   " The note would look.  We can change the unit to be a sixteenth note (a semiquaver) if we use the L (unit note length) header",
   " This will have the effect of doubling the speed.")));
   var xmplAccidentals = "K: AMinor \r\n| A2 B^c dcBc [CEa] |";
   var instAccidentals = A2($Basics._op["++"],
   "Similarly, you can sharpen a note by placing a caret symbol (^) immediately before it and flatten it using an underscore",
   A2($Basics._op["++"],
   " symbol (_). If you need a double sharp or double flat, then just double the appropriate symbol.",
   " This example reverts the major feel although the key is now A Minor. Each C is sharpened."));
   var xmplNaturals = "K: AMajor \r\n| A2 B=c dcBc [CEa] |";
   var instNaturals = A2($Basics._op["++"],
   "If your key means that certain notes are sharpened or flattened, but you need to play the \'natural\' ",
   A2($Basics._op["++"],
   " (unsharpened or unflattened) note, then you can override the key by using an equals symbol immediately before the note.",
   A2($Basics._op["++"],
   " Remember that, as in a score, you only need to mark as natural the first occurrence of the note in any given bar.",
   " For example, this reverts the previous tune to a minor feel although the key is still a major one. Each C is natural.")));
   var xmplFlatKeySig = "K: Bb\r\n| BfdB AecA | FdBF D4 |";
   var instFlatKeySig = A2($Basics._op["++"],
   "If your key is a major key, you can, if you want, leave out the word \'Major\'.  If it is a flat key, you use \'b\' and if a sharp key, \'#\'. ",
   " You can also choose to shorted the mode name to just three letters - in this case, BbMaj.");
   var xmplKeySig = "K: AMajor \r\n| A2 Bc dcBc [CEa] |";
   var instKeySig = A2($Basics._op["++"],
   "So far, we have only used the white notes on the piano - i.e. the tune snippets have tended to be in the keys either",
   A2($Basics._op["++"],
   " of C Major or A Minor.  We now introduce our first header - K: for Key Signature.  Headers are placed on lines on their own",
   A2($Basics._op["++"],
   " before the melody.  In this way, we can move the last example from A Minor to A Major. This, of course, has the effect of",
   " sharpening every C,F and G.")));
   var xmplChords = "A2 Bc dcBc [CEa]";
   var instChords = "You can play a chord by placing a group of notes, beamed together, inside square brackets - for example [CEa].";
   var xmplStrathspey = "| G | c2 e>c G<c e>g | c\'2 b>c\' a<c\' g>e |";
   var instStrathspey = A2($Basics._op["++"],
   "Conversely, you can shorten the first note of a pair and lengthen the second by means of the < character.",
   " This rhythm is found in strathspeys.");
   var hintHornpipe = "If you know it, can you finish off the \'A\' part of the tune?";
   var xmplHornpipe = "| C>GE>G C>GE>G | c>de>d c>BA>G |";
   var instHornpipe = A2($Basics._op["++"],
   "The last example was in a hornpipe-like rhythm.  Because this is so common, there is a shorthand for it",
   A2($Basics._op["++"],
   " where you separate each pair of notes with the > character.  This extends the first note by half its length",
   " and reduces the second by the same amount."));
   var xmplShortNotes = "| C3/2 G1/2 E3/2 G1/2 C3/2 G/ E3/2 G/ |";
   var instShortNotes = A2($Basics._op["++"],
   "You can shorten a note by placing a fraction after the note.  This could be, for example,",
   A2($Basics._op["++"],
   " 1/2 or 1/3. A shorthand for 1/2 is simply / and a shorthand for 1/3 is simply /3.",
   " You can extend rests the same way."));
   var hintLongNotes = "Try making some of these notes even longer.";
   var xmplLongNotes = "| F G A B c4 g2 b2 c\'4 |";
   var instLongNotes = A2($Basics._op["++"],
   "So far, all our notes have had a duration of 1 unit.  You can extend this by placing a whole number",
   " after the note (and after the octave marker if you have one).");
   var xmplOctaves = "| C,, G,, C, G, | C G c g | c\' g\' c\'\' |";
   var instOctaves = A2($Basics._op["++"],
   "You can reach octaves below middle C by adding one (or more) commas immediately after the note.",
   " Similarly higher octaves can be reached using apostrophes.");
   var hintBarsAndRests = "Try adding another bar which contains both notes and rests.";
   var xmplBarsAndRests = A2($Basics._op["++"],
   "| ABc z z def |\r\n",
   "| g z z z a |");
   var instBarsAndRests = A2($Basics._op["++"],
   "Use the character z to represent a rest.  You can set the length of a rest in exactly the same manner as for a note - by adding a number after it",
   " - for example z4. Use a vertical bar to introduce a bar line. You can spread out into multiple lines if you like.");
   var hintNotes = "Try altering some of the notes.";
   var xmplNotes = "A B c def";
   var instNotes = A2($Basics._op["++"],
   "Use the characters A-G for the notes of the octave starting from middle C and a-g for the octave above.",
   A2($Basics._op["++"],
   " You can place notes next to each other or separate them with spaces - it won\'t make much difference to ",
   " the sound but neighbouring notes will be \'beamed\' together in a score."));
   var lessons = $Array.fromList(_U.list([{title: "the notes"
                                          ,instruction: instNotes
                                          ,example: xmplNotes
                                          ,hint: hintNotes}
                                         ,{title: "bars and rests"
                                          ,instruction: instBarsAndRests
                                          ,example: xmplBarsAndRests
                                          ,hint: hintBarsAndRests}
                                         ,{title: "octaves"
                                          ,instruction: instOctaves
                                          ,example: xmplOctaves
                                          ,hint: ""}
                                         ,{title: "long notes"
                                          ,instruction: instLongNotes
                                          ,example: xmplLongNotes
                                          ,hint: hintLongNotes}
                                         ,{title: "short notes"
                                          ,instruction: instShortNotes
                                          ,example: xmplShortNotes
                                          ,hint: ""}
                                         ,{title: "hornpipes"
                                          ,instruction: instHornpipe
                                          ,example: xmplHornpipe
                                          ,hint: hintHornpipe}
                                         ,{title: "strathspeys"
                                          ,instruction: instStrathspey
                                          ,example: xmplStrathspey
                                          ,hint: ""}
                                         ,{title: "chords"
                                          ,instruction: instChords
                                          ,example: xmplChords
                                          ,hint: ""}
                                         ,{title: "key signature"
                                          ,instruction: instKeySig
                                          ,example: xmplKeySig
                                          ,hint: ""}
                                         ,{title: "sharp and flat key signatures"
                                          ,instruction: instFlatKeySig
                                          ,example: xmplFlatKeySig
                                          ,hint: ""}
                                         ,{title: "naturals"
                                          ,instruction: instNaturals
                                          ,example: xmplNaturals
                                          ,hint: ""}
                                         ,{title: "sharps and flats"
                                          ,instruction: instAccidentals
                                          ,example: xmplAccidentals
                                          ,hint: ""}
                                         ,{title: "how long is a unit note?"
                                          ,instruction: instUnitNote
                                          ,example: xmplUnitNote
                                          ,hint: ""}
                                         ,{title: "tempo"
                                          ,instruction: instTempo
                                          ,example: xmplTempo
                                          ,hint: ""}
                                         ,{title: "meter"
                                          ,instruction: instMeter
                                          ,example: xmplMeter
                                          ,hint: ""}
                                         ,{title: "tie",instruction: instTie,example: xmplTie,hint: ""}
                                         ,{title: "triplet"
                                          ,instruction: instTriplet
                                          ,example: xmplTriplet
                                          ,hint: ""}
                                         ,{title: "triplet with differing note lengths"
                                          ,instruction: instComplexTriplet
                                          ,example: xmplComplexTriplet
                                          ,hint: ""}
                                         ,{title: "quadruplet"
                                          ,instruction: instQuadruplet
                                          ,example: xmplQuadruplet
                                          ,hint: ""}
                                         ,{title: "repeats"
                                          ,instruction: instRepeat
                                          ,example: xmplRepeat
                                          ,hint: ""}
                                         ,{title: "repeats with variant endings"
                                          ,instruction: instRepeatVariants
                                          ,example: xmplRepeatVariants
                                          ,hint: ""}
                                         ,{title: "tune title"
                                          ,instruction: instTitle
                                          ,example: xmplTitle
                                          ,hint: ""}
                                         ,{title: "rhythm"
                                          ,instruction: instRhythm
                                          ,example: xmplRhythm
                                          ,hint: ""}
                                         ,{title: "information headers"
                                          ,instruction: instInformation
                                          ,example: xmplInformation
                                          ,hint: ""}
                                         ,{title: "key changes"
                                          ,instruction: instChangeKey
                                          ,example: xmplChangeKey
                                          ,hint: ""}
                                         ,{title: "other modes"
                                          ,instruction: instMixolydian
                                          ,example: xmplMixolydian
                                          ,hint: ""}
                                         ,{title: "klezmer"
                                          ,instruction: instKlezmer
                                          ,example: xmplKlezmer
                                          ,hint: ""}
                                         ,{title: "Balkan"
                                          ,instruction: instBalkan
                                          ,example: xmplBalkan
                                          ,hint: ""}]));
   var Lesson = F4(function (a,b,c,d) {
      return {title: a,instruction: b,example: c,hint: d};
   });
   return _elm.Lessons.values = {_op: _op
                                ,lessons: lessons
                                ,Lesson: Lesson};
};
Elm.AbcTutorial = Elm.AbcTutorial || {};
Elm.AbcTutorial.make = function (_elm) {
   "use strict";
   _elm.AbcTutorial = _elm.AbcTutorial || {};
   if (_elm.AbcTutorial.values) return _elm.AbcTutorial.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Abc = Elm.Abc.make(_elm),
   $AbcPerformance = Elm.AbcPerformance.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $Lessons = Elm.Lessons.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Melody = Elm.Melody.make(_elm),
   $Notable = Elm.Notable.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $SoundFont = Elm.SoundFont.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var pianoFonts = $SoundFont.loadSoundFont("acoustic_grand_piano");
   var highlights = function (model) {
      var mpe = model.error;
      var _p0 = mpe;
      if (_p0.ctor === "Nothing") {
            return _U.list([]);
         } else {
            var _p1 = _p0._0;
            return _U.cmp($String.length(model.abc),
            _p1.position) > 0 ? _U.list([A2($Html$Attributes.property,
                                        "selectionStart",
                                        $Json$Encode.string($Basics.toString(_p1.position)))
                                        ,A2($Html$Attributes.property,
                                        "selectionEnd",
                                        $Json$Encode.string($Basics.toString(_p1.position + 1)))
                                        ,A2($Html$Attributes.property,
                                        "focus",
                                        $Json$Encode.$null)]) : _U.list([]);
         }
   };
   var legendStyle = $Html$Attributes.style(_U.list([{ctor: "_Tuple2"
                                                     ,_0: "background-color"
                                                     ,_1: "#67d665"}
                                                    ,{ctor: "_Tuple2",_0: "border-top",_1: "1px solid #d4d4d4"}
                                                    ,{ctor: "_Tuple2",_0: "border-bottom",_1: "1px solid #d4d4d4"}
                                                    ,{ctor: "_Tuple2",_0: "-moz-box-shadow",_1: "3px 3px 3px #ccc"}
                                                    ,{ctor: "_Tuple2"
                                                     ,_0: "-webkit-box-shadow"
                                                     ,_1: "3px 3px 3px #ccc"}
                                                    ,{ctor: "_Tuple2",_0: "box-shadow",_1: "3px 3px 3px #ccc"}
                                                    ,{ctor: "_Tuple2",_0: "font-size",_1: "1em"}
                                                    ,{ctor: "_Tuple2",_0: "padding",_1: "0.3em 1em"}]));
   var fieldsetStyle = $Html$Attributes.style(_U.list([{ctor: "_Tuple2"
                                                       ,_0: "background-color"
                                                       ,_1: "#f1f1f1"}
                                                      ,{ctor: "_Tuple2",_0: "border",_1: "none"}
                                                      ,{ctor: "_Tuple2",_0: "border-radius",_1: "2px"}
                                                      ,{ctor: "_Tuple2",_0: "margin-bottom",_1: "12px"}
                                                      ,{ctor: "_Tuple2",_0: "padding",_1: "10px 10px 20px 10px"}
                                                      ,{ctor: "_Tuple2",_0: "display",_1: "inline-block"}]));
   var bStyle = function (disabled) {
      var colour = disabled ? _U.list([{ctor: "_Tuple2"
                                       ,_0: "background-color"
                                       ,_1: "#7D7C7C"}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "color"
                                       ,_1: "grey"}]) : _U.list([{ctor: "_Tuple2"
                                                                 ,_0: "background-color"
                                                                 ,_1: "#67d665"}
                                                                ,{ctor: "_Tuple2"
                                                                 ,_0: "background"
                                                                 ,_1: "-webkit-gradient(linear, left top, left bottom, from(#3e9c5f), to(#67d665))"}
                                                                ,{ctor: "_Tuple2"
                                                                 ,_0: "background"
                                                                 ,_1: "-webkit-linear-gradient(top, #3e9c5f, #67d665)"}
                                                                ,{ctor: "_Tuple2"
                                                                 ,_0: "background"
                                                                 ,_1: "-moz-linear-gradient(top, #3e9c5f, #67d665)"}
                                                                ,{ctor: "_Tuple2"
                                                                 ,_0: "background"
                                                                 ,_1: "-ms-linear-gradient(top, #3e9c5f, #67d665)"}
                                                                ,{ctor: "_Tuple2"
                                                                 ,_0: "background"
                                                                 ,_1: "-o-linear-gradient(top, #3e9c5f, #67d665)"}
                                                                ,{ctor: "_Tuple2",_0: "color",_1: "black"}]);
      var basecss = _U.list([{ctor: "_Tuple2"
                             ,_0: "border-top"
                             ,_1: "1px solid #97d9f7"}
                            ,{ctor: "_Tuple2",_0: "padding",_1: "5px 10px"}
                            ,{ctor: "_Tuple2",_0: "-webkit-border-radius",_1: "8px"}
                            ,{ctor: "_Tuple2",_0: "-moz-border-radius",_1: "8px"}
                            ,{ctor: "_Tuple2",_0: "border-radius",_1: "8px"}
                            ,{ctor: "_Tuple2"
                             ,_0: "-webkit-box-shadow"
                             ,_1: "rgba(0,0,0,1) 0 1px 0"}
                            ,{ctor: "_Tuple2"
                             ,_0: "-moz-box-shadow"
                             ,_1: "rgba(0,0,0,1) 0 1px 0"}
                            ,{ctor: "_Tuple2",_0: "box-shadow",_1: "rgba(0,0,0,1) 0 1px 0"}
                            ,{ctor: "_Tuple2"
                             ,_0: "text-shadow"
                             ,_1: "rgba(0,0,0,.4) 0 1px 0"}
                            ,{ctor: "_Tuple2",_0: "font-size",_1: "14px"}
                            ,{ctor: "_Tuple2",_0: "font-family",_1: "Georgia, serif"}
                            ,{ctor: "_Tuple2",_0: "text-decoration",_1: "none"}
                            ,{ctor: "_Tuple2",_0: "vertical-align",_1: "middle"}
                            ,{ctor: "_Tuple2",_0: "margin",_1: "10px 5px 10px 5px"}
                            ,{ctor: "_Tuple2"
                             ,_0: "font"
                             ,_1: "100% \"Trebuchet MS\", Verdana, sans-serif"}]);
      return $Html$Attributes.style(A2($Basics._op["++"],
      basecss,
      colour));
   };
   var centreStyle = $Html$Attributes.style(_U.list([{ctor: "_Tuple2"
                                                     ,_0: "text-align"
                                                     ,_1: "center"}
                                                    ,{ctor: "_Tuple2",_0: "margin",_1: "auto"}]));
   var instructionStyle = $Html$Attributes.style(_U.list([{ctor: "_Tuple2"
                                                          ,_0: "padding"
                                                          ,_1: "10px 0"}
                                                         ,{ctor: "_Tuple2",_0: "border",_1: "none"}
                                                         ,{ctor: "_Tuple2",_0: "text-align",_1: "left"}
                                                         ,{ctor: "_Tuple2",_0: "align",_1: "center"}
                                                         ,{ctor: "_Tuple2",_0: "display",_1: "block"}
                                                         ,{ctor: "_Tuple2",_0: "margin-left",_1: "auto"}
                                                         ,{ctor: "_Tuple2",_0: "margin-right",_1: "auto"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "font"
                                                          ,_1: "100% \"Trebuchet MS\", Verdana, sans-serif"}]));
   var taStyle = $Html$Attributes.style(_U.list([{ctor: "_Tuple2"
                                                 ,_0: "padding"
                                                 ,_1: "10px 0"}
                                                ,{ctor: "_Tuple2",_0: "font-size",_1: "1.5em"}
                                                ,{ctor: "_Tuple2",_0: "text-align",_1: "left"}
                                                ,{ctor: "_Tuple2",_0: "align",_1: "center"}
                                                ,{ctor: "_Tuple2",_0: "display",_1: "block"}
                                                ,{ctor: "_Tuple2",_0: "margin-left",_1: "auto"}
                                                ,{ctor: "_Tuple2",_0: "margin-right",_1: "auto"}]));
   var viewError = function (me) {
      var _p2 = me;
      if (_p2.ctor === "Nothing") {
            return "";
         } else {
            return $Abc.parseError(_p2._0);
         }
   };
   var hint = function (i) {
      var mlesson = A2($Array.get,i,$Lessons.lessons);
      var _p3 = mlesson;
      if (_p3.ctor === "Nothing") {
            return "";
         } else {
            return _p3._0.hint;
         }
   };
   var example = function (i) {
      var mlesson = A2($Array.get,i,$Lessons.lessons);
      var _p4 = mlesson;
      if (_p4.ctor === "Nothing") {
            return "error";
         } else {
            return _p4._0.example;
         }
   };
   var instruction = function (i) {
      var mlesson = A2($Array.get,i,$Lessons.lessons);
      var _p5 = mlesson;
      if (_p5.ctor === "Nothing") {
            return "error";
         } else {
            return _p5._0.instruction;
         }
   };
   var title = function (i) {
      var mlesson = A2($Array.get,i,$Lessons.lessons);
      var _p6 = mlesson;
      if (_p6.ctor === "Nothing") {
            return "error";
         } else {
            return A2($Basics._op["++"],
            $Basics.toString(i + 1),
            A2($Basics._op["++"]," - ",_p6._0.title));
         }
   };
   var toPerformance = function (ml) {
      var melody = A2($Debug.log,"melody",ml);
      return A2($Result.map,$Notable.fromMelodyLine(0.0),melody);
   };
   var toInt = function (_p7) {
      return A2($Maybe.withDefault,
      0,
      $Result.toMaybe($String.toInt(_p7)));
   };
   var terminateLine = function (s) {
      return A2($Basics._op["++"],s,"\r\n");
   };
   var performanceDuration = function (rp) {
      var notes = A2($Debug.log,
      "performance notes",
      A2($Result.withDefault,_U.list([]),rp));
      var maybeLastNote = $List.head(notes);
      var _p8 = maybeLastNote;
      if (_p8.ctor === "Nothing") {
            return A2($Debug.log,"nothing",0.0);
         } else {
            return A2($Debug.log,"Just",$Basics.fst(_p8._0));
         }
   };
   var nextSound = F2(function (samples,ne) {
      var _p9 = ne;
      var time = _p9._0;
      var notable = _p9._1;
      var _p10 = notable;
      var sample = A2($Dict.get,_p10._0,samples);
      var soundBite = {mss: sample,time: time,gain: _p10._1};
      return $SoundFont.maybePlay(soundBite);
   });
   var makeSounds = F2(function (ss,perfResult) {
      var _p11 = perfResult;
      if (_p11.ctor === "Ok") {
            return A2($List.map,nextSound(ss),_p11._0);
         } else {
            return _U.list([]);
         }
   });
   var Error = function (a) {    return {ctor: "Error",_0: a};};
   var returnError = function (e) {
      return $Effects.task($Task.succeed(Error(e)));
   };
   var MoveToEnd = function (a) {
      return {ctor: "MoveToEnd",_0: a};
   };
   var Move = function (a) {    return {ctor: "Move",_0: a};};
   var ShowButtons = {ctor: "ShowButtons"};
   var showButtons = $Task.succeed(ShowButtons);
   var showButtonsAction = $Effects.task(showButtons);
   var Play = {ctor: "Play"};
   var Abc = function (a) {    return {ctor: "Abc",_0: a};};
   var view = F2(function (address,model) {
      return A2($Html.div,
      _U.list([centreStyle]),
      _U.list([A2($Html.h1,
              _U.list([]),
              _U.list([$Html.text(title(model.lessonIndex))]))
              ,A2($Html.textarea,
              _U.list([$Html$Attributes.value(instruction(model.lessonIndex))
                      ,instructionStyle
                      ,$Html$Attributes.readonly(true)
                      ,$Html$Attributes.cols(96)
                      ,$Html$Attributes.rows(6)]),
              _U.list([]))
              ,A2($Html.fieldset,
              _U.list([fieldsetStyle]),
              _U.list([A2($Html.legend,
                      _U.list([legendStyle]),
                      _U.list([$Html.text("you can edit the text inside the box and then hit play")]))
                      ,A2($Html.textarea,
                      A2($Basics._op["++"],
                      _U.list([$Html$Attributes.placeholder("abc")
                              ,$Html$Attributes.value(model.abc)
                              ,A3($Html$Events.on,
                              "input",
                              $Html$Events.targetValue,
                              function (a) {
                                 return A2($Signal.message,address,Abc(a));
                              })
                              ,taStyle
                              ,$Html$Attributes.cols(70)
                              ,$Html$Attributes.rows(13)
                              ,$Html$Attributes.autocomplete(false)
                              ,$Html$Attributes.spellcheck(false)
                              ,$Html$Attributes.autofocus(true)]),
                      highlights(model)),
                      _U.list([]))]))
              ,A2($Html.div,
              _U.list([centreStyle]),
              _U.list([A2($Html.button,
                      _U.list([bStyle(model.buttonsDisabled)
                              ,A2($Html$Events.onClick,address,MoveToEnd(false))
                              ,$Html$Attributes.disabled(model.buttonsDisabled)]),
                      _U.list([$Html.text("first")]))
                      ,A2($Html.button,
                      _U.list([bStyle(model.buttonsDisabled)
                              ,A2($Html$Events.onClick,address,Move(false))
                              ,$Html$Attributes.disabled(model.buttonsDisabled)]),
                      _U.list([$Html.text("previous")]))
                      ,A2($Html.button,
                      _U.list([bStyle(model.buttonsDisabled)
                              ,A2($Html$Events.onClick,address,Play)
                              ,$Html$Attributes.disabled(model.buttonsDisabled)]),
                      _U.list([$Html.text("play")]))
                      ,A2($Html.button,
                      _U.list([bStyle(model.buttonsDisabled)
                              ,A2($Html$Events.onClick,address,Move(true))
                              ,$Html$Attributes.disabled(model.buttonsDisabled)]),
                      _U.list([$Html.text("next")]))
                      ,A2($Html.button,
                      _U.list([bStyle(model.buttonsDisabled)
                              ,A2($Html$Events.onClick,address,MoveToEnd(true))
                              ,$Html$Attributes.disabled(model.buttonsDisabled)]),
                      _U.list([$Html.text("last")]))]))
              ,A2($Html.div,
              _U.list([centreStyle]),
              _U.list([A2($Html.p,
                      _U.list([]),
                      _U.list([$Html.text(hint(model.lessonIndex))]))
                      ,A2($Html.p,
                      _U.list([]),
                      _U.list([$Html.text(viewError(model.error))]))]))]));
   });
   var LoadFont = function (a) {
      return {ctor: "LoadFont",_0: a};
   };
   var signals = _U.list([A2($Signal.map,LoadFont,pianoFonts)]);
   var NoOp = {ctor: "NoOp"};
   var suspend = function (rp) {
      var time = performanceDuration(rp) * 1000;
      return A2($Task.andThen,
      $Task.sleep(time),
      function (_p12) {
         return $Task.succeed(NoOp);
      });
   };
   var playAndSuspend = F2(function (rp,sounds) {
      return A2($Task.andThen,
      $Task.sequence(sounds),
      function (_p13) {
         return suspend(rp);
      });
   });
   var playSounds = F2(function (rp,sounds) {
      return $Effects.task(A2($Task.map,
      function (_p14) {
         return ShowButtons;
      },
      A2(playAndSuspend,rp,sounds)));
   });
   var playAbc = function (m) {
      var pr = toPerformance($AbcPerformance.melodyFromAbcResult($Abc.parse(terminateLine(m.abc))));
      var _p15 = pr;
      if (_p15.ctor === "Ok") {
            return A2(playSounds,pr,A2(makeSounds,m.samples,pr));
         } else {
            return returnError(_p15._0);
         }
   };
   var update = F2(function (action,model) {
      var _p16 = action;
      switch (_p16.ctor)
      {case "NoOp": return {ctor: "_Tuple2"
                           ,_0: model
                           ,_1: $Effects.none};
         case "ShowButtons": return {ctor: "_Tuple2"
                                    ,_0: _U.update(model,{buttonsDisabled: false})
                                    ,_1: $Effects.none};
         case "LoadFont": var _p17 = _p16._0;
           if (_p17.ctor === "Nothing") {
                 return {ctor: "_Tuple2",_0: model,_1: $Effects.none};
              } else {
                 var _p19 = _p17._0;
                 var _p18 = _p19.name;
                 if (_p18 === "end") {
                       return {ctor: "_Tuple2"
                              ,_0: _U.update(model,{loaded: true})
                              ,_1: showButtonsAction};
                    } else {
                       var pitch = toInt(_p19.name);
                       return {ctor: "_Tuple2"
                              ,_0: _U.update(model,
                              {samples: A3($Dict.insert,pitch,_p19,model.samples)})
                              ,_1: $Effects.none};
                    }
              }
         case "Abc": return {ctor: "_Tuple2"
                            ,_0: _U.update(model,{abc: _p16._0})
                            ,_1: $Effects.none};
         case "Play": return {ctor: "_Tuple2"
                             ,_0: _U.update(model,
                             {error: $Maybe.Nothing,buttonsDisabled: true})
                             ,_1: playAbc(model)};
         case "Move": var next = function () {
              var _p20 = _p16._0;
              if (_p20 === true) {
                    return A2($Basics.min,
                    model.lessonIndex + 1,
                    $Array.length($Lessons.lessons) - 1);
                 } else {
                    return A2($Basics.max,model.lessonIndex - 1,0);
                 }
           }();
           return {ctor: "_Tuple2"
                  ,_0: _U.update(model,
                  {lessonIndex: next,abc: example(next),error: $Maybe.Nothing})
                  ,_1: $Effects.none};
         case "MoveToEnd": var next = function () {
              var _p21 = _p16._0;
              if (_p21 === true) {
                    return $Array.length($Lessons.lessons) - 1;
                 } else {
                    return 0;
                 }
           }();
           return {ctor: "_Tuple2"
                  ,_0: _U.update(model,
                  {lessonIndex: next,abc: example(next),error: $Maybe.Nothing})
                  ,_1: $Effects.none};
         default: return {ctor: "_Tuple2"
                         ,_0: _U.update(model,{error: $Maybe.Just(_p16._0)})
                         ,_1: showButtonsAction};}
   });
   var init = function (topic) {
      return {ctor: "_Tuple2"
             ,_0: {samples: $Dict.empty
                  ,loaded: false
                  ,abc: example(0)
                  ,lessonIndex: 0
                  ,error: $Maybe.Nothing
                  ,buttonsDisabled: true}
             ,_1: $Effects.none};
   };
   var Model = F6(function (a,b,c,d,e,f) {
      return {samples: a
             ,loaded: b
             ,abc: c
             ,lessonIndex: d
             ,error: e
             ,buttonsDisabled: f};
   });
   return _elm.AbcTutorial.values = {_op: _op
                                    ,Model: Model
                                    ,init: init
                                    ,NoOp: NoOp
                                    ,LoadFont: LoadFont
                                    ,Abc: Abc
                                    ,Play: Play
                                    ,ShowButtons: ShowButtons
                                    ,Move: Move
                                    ,MoveToEnd: MoveToEnd
                                    ,Error: Error
                                    ,update: update
                                    ,nextSound: nextSound
                                    ,makeSounds: makeSounds
                                    ,playSounds: playSounds
                                    ,playAndSuspend: playAndSuspend
                                    ,suspend: suspend
                                    ,showButtons: showButtons
                                    ,showButtonsAction: showButtonsAction
                                    ,performanceDuration: performanceDuration
                                    ,returnError: returnError
                                    ,terminateLine: terminateLine
                                    ,toInt: toInt
                                    ,toPerformance: toPerformance
                                    ,playAbc: playAbc
                                    ,title: title
                                    ,instruction: instruction
                                    ,example: example
                                    ,hint: hint
                                    ,viewError: viewError
                                    ,view: view
                                    ,taStyle: taStyle
                                    ,instructionStyle: instructionStyle
                                    ,centreStyle: centreStyle
                                    ,bStyle: bStyle
                                    ,fieldsetStyle: fieldsetStyle
                                    ,legendStyle: legendStyle
                                    ,highlights: highlights
                                    ,pianoFonts: pianoFonts
                                    ,signals: signals};
};
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm),
   $AbcTutorial = Elm.AbcTutorial.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $StartApp = Elm.StartApp.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var app = $StartApp.start({init: $AbcTutorial.init("abc tutorial")
                             ,update: $AbcTutorial.update
                             ,view: $AbcTutorial.view
                             ,inputs: $AbcTutorial.signals});
   var main = app.html;
   var tasks = Elm.Native.Task.make(_elm).performSignal("tasks",
   app.tasks);
   return _elm.Main.values = {_op: _op,app: app,main: main};
};
