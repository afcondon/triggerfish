// Stellatus SuperDirt daemon — headless boot for the Triggerfish rig.
//
// Boots scsynth + SuperDirt with no IDE, loads the Dirt-Samples quark, and
// listens for /dirt/play OSC from the Stellatus bridge (audio/stellatus-bridge.mjs).
//
// Port comes from the environment (SUPERDIRT_PORT), defaulting to 57135 — NOT
// 57120, which es9-daemon owns on the MBP (see docs/SUFFLAMEN-DESIGN.md
// §"The SuperDirt daemon under Bosun"). This is the port-from-env contract the
// bosun-daemon skill wants.
//
// Run headless:   SUPERDIRT_PORT=57135 /Applications/SuperCollider.app/Contents/MacOS/sclang audio/superdirt-daemon.scd
// or via:         audio/boot-superdirt.sh
//
// Readiness: once booted it prints "STELLATUS-SUPERDIRT READY on port N"; the
// UDP port is then bound (lsof -nP -iUDP:N).

(
var port = ("SUPERDIRT_PORT".getenv ? "57135").asInteger;
s.reboot {
	// Server options (mirrors the SuperDirt example; only applied on reboot).
	s.options.numBuffers = 1024 * 256;
	s.options.memSize = 8192 * 64;
	s.options.numWireBufs = 2048;
	s.options.maxNodes = 1024 * 32;
	s.options.numOutputBusChannels = 2;
	s.options.numInputBusChannels = 2;

	s.waitForBoot {
		~dirt.stop;                       // avoid duplicate dirt on reboot
		~dirt = SuperDirt(2, s);
		~dirt.loadSoundFiles;             // the Dirt-Samples quark (808bd, sn, hh27, cp, …)
		s.sync;
		~dirt.start(port, 0 ! 12);        // 12 orbits, all to channel 0
		SuperDirt.default = ~dirt;
		("STELLATUS-SUPERDIRT READY on port " ++ port).postln;
	};

	s.latency = 0.3;
};
);
