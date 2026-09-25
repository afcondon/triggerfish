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
//
// NB: NO outer ( … ) wrapper. `sclang file.scd` runs the file at the command-line
// top level, where `var` declarations are legal but a `( var …; … )` paren-block
// is NOT (it fails to parse — "unexpected ';'"). SuperCollider's user startup.scd
// also auto-boots a SuperDirt on 57120; this file's `s.reboot` then re-boots the
// server with OUR options (port + device) on top, which is what makes it stick.

var port = ("SUPERDIRT_PORT".getenv ? "57135").asInteger;
// Optional CoreAudio device for scsynth I/O. Unset = the system default output
// (you hear SuperDirt directly). Set SUPERDIRT_DEVICE="BlackHole 2ch" to route a
// stereo pair into a virtual device so Ableton can take it as an input (capture /
// mix / record). BlackHole 2ch is full-duplex, so one device string covers I/O.
var dev = "SUPERDIRT_DEVICE".getenv;

// --- output pairs for send buses (SUPERDIRT_OUTPUTS) --------------------------
// How many output channels scsynth opens on the device. 2 (default) is the old
// behaviour: every orbit on 1/2. With a device that has more — BlackHole 16ch —
// the Conspicillum SEND orbits get their own pairs, so Ableton can take each as
// a return track and put a real reverb or delay on it:
//
//   orbits 0-9   -> outputs 1/2   (the main mix, as always)
//   orbit 10     -> outputs 3/4   (send A)  when SUPERDIRT_OUTPUTS >= 4
//   orbit 11     -> outputs 5/6   (send B)  when SUPERDIRT_OUTPUTS >= 6
//
// A send in Conspicillum is a COPY of the grain at a send level (OpSend), so
// the return in Ableton wants to be 100% wet: the dry grain is already on 1/2.
var outs = ("SUPERDIRT_OUTPUTS".getenv ? "2").asInteger.max(2);
var orbitOuts = (0 ! 10) ++ [ if(outs >= 4) { 2 } { 0 }, if(outs >= 6) { 4 } { 0 } ];

// --- which samples to load (SUPERDIRT_SAMPLES) -------------------------------
// Loading all 217 Dirt-Samples banks costs ~935 MB of scsynth footprint (394 MB
// on disk, ~2x that as float32 buffers). That was the rig's single largest
// memory holder and it is almost all banks this rig never names. So the sample
// set is now port-from-env like the port and device:
//
//   core  (default)  the banks below, fully read. ~9 MB on disk. Every sample is
//                    in memory before the first beat, so no first-hit surprise.
//                    This is the one to run under Bosun and in performance.
//   full             all 217 banks, fully read. The old behaviour. Reach for it
//                    when digging through the library for sounds.
//   lazy             all 217 banks REGISTERED but header-only, audio read on
//                    first use (SuperDirt's own doNotReadYet). Every name
//                    resolves and it stays small — but readFileIfNecessary is
//                    ASYNC, so the first hit of a given sample fires the read
//                    and returns before the buffer is ready: you lose that one
//                    note. Fine for browsing, NOT for a take.
//
// Stellatus' KIT field (`name = sample:index`) accepts any Dirt-Samples folder,
// so under `core` a name outside this list resolves to nothing and stays silent.
// Add it here — or boot `full`/`lazy` — if you go looking further afield.
var sampleSet = ("SUPERDIRT_SAMPLES".getenv ? "core").toLower;
var sampleDir = "SUPERDIRT_SAMPLES_DIR".getenv
	? (Platform.userAppSupportDir +/+ "downloaded-quarks/Dirt-Samples");
var coreBanks = [
	"bd", "sn", "sd", "hh", "hc", "ho", "oh", "cp", "cr", "rd",
	"808", "808bd", "808sd", "808cy", "drum", "bass", "arpy", "feel"
];
// NB loadSoundFiles only pathMatches when handed a STRING; an ARRAY is taken as
// literal folder paths and then filtered by `endsWith("/")`. So glob each name
// ourselves: pathMatch returns a trailing-slash path for a folder that exists
// and an empty array for one that doesn't, which both satisfies that filter and
// drops any bank not installed (rather than failing the whole load on it).
var coreFolders = coreBanks.collect { |b| (sampleDir +/+ b ++ "/").pathMatch }.flatten;
// --- takes recorded on the guitar (ITAJARA_TAKES_DIR) -------------------------
// Itajara writes each take as a directory of one WAV per layer, so a take IS a
// sample bank and its layers ARE the bank's indices: `sound "mytake"` plays
// layer 0, `n 1` plays layer 1. Nothing had to be invented for that — it falls
// out of both tools agreeing that a folder of files is a named, indexed set.
//
// Which means a take's DIRECTORY NAME is the name you type in a pattern. Name
// takes accordingly.
//
// Loaded in every sample set, including `core`: these are a handful of files
// the player deliberately made, not a 217-bank library, and a take you recorded
// two minutes ago being silent because the rig booted `core` would be the least
// explicable silence on the rig.
var takesDir = "ITAJARA_TAKES_DIR".getenv ? ("HOME".getenv +/+ ".itajara/takes");
var takeFolders = (takesDir +/+ "*/").pathMatch;
// --- swept sample sets (QUADRAT_SETS_DIR) ------------------------------------
// Quadrat cuts a take into a SET — a folder of numbered WAVs plus a `set.json`
// saying what each one meant — under ~/.itajara/quadrat/samples.
//
// **Nothing had to be built to make those playable here.** A folder of files is
// a named, indexed set at both ends, so `s "morphtest6" # n 3` is the fourth
// sample and that is the whole of the SuperDirt "encoding". Which is the point
// of loading them: SuperDirt is the destination with NO constraints, and it is
// the honest test of a model built against the Rample, where twelve layers, the
// eight SLICER divisions and 16-bit mono are all hard.
//
// Two things make it work rather than nearly work, and both were checked
// against SuperDirt's own source rather than assumed:
//
//   `set.json` is IGNORED, not misread. DirtSoundLibrary.readSoundFile filters
//   on extension (wav/aif/aiff/aifc) and returns nil for anything else, and
//   loadSoundFilePaths only adds a buffer when it is non-nil. So the
//   description sitting in the folder does not shift a single index — which it
//   would have done silently, and which is the reason to check rather than hope.
//
//   The files are `<set>-01.wav`, ZERO-PADDED, and loadSoundFileFolder sorts.
//   Unpadded, the tenth sample would sort between the first and the second and
//   every n past it would name the wrong audio.
//
// A set's DIRECTORY NAME is the name you type. Loaded after the core banks and
// before nothing, so a set called `bd` REPLACES the bank called `bd` — name
// sets for what they are and the collision does not arise.
var setsDir = "QUADRAT_SETS_DIR".getenv ? ("HOME".getenv +/+ ".itajara/quadrat/samples");
var setFolders = (setsDir +/+ "*/").pathMatch;

s.reboot {
	// Server options (only applied on reboot).
	//
	// These were the SuperDirt example's values, which are sized for "load every
	// quark anyone might ever want" and cost ~965 MB of phys_footprint on this
	// 16 GB machine — reserved at boot and almost none of it touched (RSS measured
	// at 28 MB). Sized to THIS rig instead, 2026-08-05:
	//
	//   numBuffers  one buffer per loaded sample. Dirt-Samples is 2017 files
	//               today, so 8192 is 4x headroom. (Was 262144 = 130x the sample
	//               count.) RAISE THIS if loadSoundFiles ever logs "no more
	//               buffer numbers" — that is the symptom of too few.
	//   memSize     scsynth's real-time allocation pool, in KB — the delays,
	//               reverbs and FFT chains draw from it. 128 MB is 16x the SC
	//               default, for 12 orbits. (Was 512 MB.) THIS is the setting
	//               that dominated the footprint. Symptom of too little is
	//               "alloc failed, increase server's memory allocation".
	//   maxNodes    4096 synth/group nodes. (Was 32768.) Symptom of too few is
	//               "too many nodes" on dense patterns.
	//   numWireBufs left at 2048 deliberately — a wire buf is blockSize floats
	//               (~256 B), so 2048 costs well under a megabyte, and cutting
	//               it risks "exceeded number of interconnect buffers" on
	//               SuperDirt's deeper effect chains. Nothing to gain.
	s.options.numBuffers = 1024 * 8;
	s.options.memSize = 8192 * 16;
	s.options.numWireBufs = 2048;
	s.options.maxNodes = 1024 * 4;
	s.options.numOutputBusChannels = outs;
	s.options.numInputBusChannels = 2;
	if((dev.notNil) and: { dev != "" }, {
		s.options.device = dev;
		("STELLATUS-SUPERDIRT device = " ++ dev).postln;
	});

	s.waitForBoot {
		~dirt.stop;                       // avoid duplicate dirt on reboot
		~dirt = SuperDirt(2, s);
		case
		{ sampleSet == "full" } {
			("STELLATUS-SUPERDIRT samples = full (all banks, ~935 MB)").postln;
			~dirt.loadSoundFiles;
		}
		{ sampleSet == "lazy" } {
			("STELLATUS-SUPERDIRT samples = lazy (all banks, header-only)").postln;
			~dirt.soundLibrary.doNotReadYet = true;
			~dirt.loadSoundFiles;
		}
		{
			// default: core. Warn rather than silently boot an empty library if
			// the quark moved — a rig with no samples should be loud about it.
			if(coreFolders.isEmpty, {
				("STELLATUS-SUPERDIRT WARNING: no core banks found under " ++ sampleDir
					++ " — set SUPERDIRT_SAMPLES_DIR, or boot with SUPERDIRT_SAMPLES=full").postln;
			}, {
				("STELLATUS-SUPERDIRT samples = core (" ++ coreFolders.size ++ " banks)").postln;
				~dirt.loadSoundFiles(coreFolders);
			});
		};
		s.sync;
		// Takes, in every sample set — see takeFolders above. Read properly even
		// when the library is lazy: doNotReadYet loses the first hit of a sample,
		// and the first hit of a take you just recorded is the one being listened
		// for.
		if(takeFolders.notEmpty, {
			~dirt.soundLibrary.doNotReadYet = false;
			("STELLATUS-SUPERDIRT takes = " ++ takeFolders.size ++ " from " ++ takesDir).postln;
			~dirt.loadSoundFiles(takeFolders);
			s.sync;
		});
		// Swept sets, in every sample set, and read properly for the same
		// reason takes are: a set you cut two minutes ago being silent on its
		// first hit would be the least explicable silence on the rig.
		if(setFolders.notEmpty, {
			~dirt.soundLibrary.doNotReadYet = false;
			("STELLATUS-SUPERDIRT sets = " ++ setFolders.size ++ " from " ++ setsDir).postln;
			~dirt.loadSoundFiles(setFolders);
			s.sync;
		});
		// --- Conspicillum resonator ------------------------------------------
		//
		// SuperDirt ships no resonator, and this is the demonstration that its
		// effect list is not a fixed menu: a SynthDef plus `~dirt.addModule`
		// puts one on exactly the same footing as `crush`, addressable per
		// grain by a `Reef.Conspicillum.Cloud` rule.
		//
		// WHY a resonator and not another filter. Every other per-event effect
		// colours the grain. A resonator makes the grain the EXCITER of
		// something else, so the pitch comes from the resonator and the timbre
		// from the recording — which is what "a tone derived from the sample"
		// means. And because a Conspicillum corpus carries the MIDI notes that
		// were really struck, the resonator can be tuned to the chord we are
		// TARGETING while excited by a grain that does not contain it: a chord
		// the material cannot answer stops being silence and becomes the chord,
		// in whatever timbre the material could offer.
		//
		// THE TAIL IS BOUNDED BY THE GRAIN, and this is the constraint that
		// shapes how the module is used. `dirt_gate` carries
		// `doneAction: 14` — free the surrounding group and every node in it —
		// after `sustain + fadeTime`. A per-event synth therefore CANNOT
		// outlive its grain, so a 2.5 s ring on a 90 ms grain is 90 ms of ring
		// and sounds like a filtered click. Nothing errors; it just does not
		// do what you asked.
		//
		// The way to use it is to open the gate wide and let the ENVELOPE make
		// the grain short: a long `sustain` with `grenvelo` tilt near 0 gives a
		// percussive burst of sample followed by the resonator singing for the
		// rest of the window. Which is, as it happens, exactly how a struck
		// instrument behaves — a short excitation and a long decay. See
		// `dirt_rsn_global` below for the version that rings freely.
		//
		// Two models, because they fail differently and both are wanted:
		//
		//   klank  a bank of ringing filters on the harmonic series, each
		//          partial with its OWN decay time. High partials dying faster
		//          than low ones is the signature of anything struck or
		//          plucked, and it is the thing no granular module on the
		//          comparison list has, because none of them has a resonator to
		//          put it in. Metallic, bell-like, expensive.
		//   pluck  Karplus-Strong. The delay line is FILLED with the grain at
		//          onset, so the string is literally strung with a slice of the
		//          chord and then allowed to decay. Cheaper, and the more
		//          string-like of the two.
		//
		// Both run the resonator in MONO and spread the result. A stereo
		// DynKlank is two banks for a difference nobody can hear on material
		// this short, and the cost of this module is the one real cost among
		// the effects — measure before assuming C1's 12,800 grains/sec survives.
		[1, 2].do { |nc|
			SynthDef("dirt_rsn_klank" ++ nc, { |out, rsnpitch = 60, rsndecay = 1.5,
					rsnbright = 0.7, rsnmix = 1|
				var parts = 10;
				var dry = In.ar(out, nc);
				var mono = if(nc > 1) { Mix.ar(dry) / nc } { dry };
				var f0 = rsnpitch.midicps;
				// Partial k rings for decay * bright^(k-1). bright -> 1 is a
				// bar that keeps its highs; bright -> 0.3 is a dull thud.
				var wet = DynKlank.ar(`[
						(1..parts) * f0,
						(1..parts).collect { |k| 1 / k },
						(1..parts).collect { |k| rsndecay * rsnbright.pow(k - 1) }
					], mono * 0.08);
				// A ringing bank fed continuously WILL run away. Soft-clip
				// rather than limit: a limiter would duck the whole grain.
				wet = LeakDC.ar(wet).tanh;
				ReplaceOut.ar(out, XFade2.ar(dry, if(nc > 1) { wet ! nc } { wet },
					rsnmix * 2 - 1));
			}, [\ir, \ir, \ir, \ir, \ir]).add;

			SynthDef("dirt_rsn_pluck" ++ nc, { |out, rsnpitch = 60, rsndecay = 1.5,
					rsnbright = 0.7, rsnmix = 1|
				var dry = In.ar(out, nc);
				var mono = if(nc > 1) { Mix.ar(dry) / nc } { dry };
				// One impulse at grain onset: Pluck fills the delay line from
				// its input at that moment, which is how the grain becomes the
				// string rather than merely feeding it.
				var wet = Pluck.ar(mono, Impulse.ar(0), 0.05,
					(1 / rsnpitch.midicps).clip(0.00002, 0.05),
					rsndecay, (1 - rsnbright) * 0.5);
				wet = LeakDC.ar(wet);
				ReplaceOut.ar(out, XFade2.ar(dry, if(nc > 1) { wet ! nc } { wet },
					rsnmix * 2 - 1));
			}, [\ir, \ir, \ir, \ir, \ir]).add;
		};
		s.sync;

		~dirt.start(port, orbitOuts);     // 12 orbits; 10 and 11 are the sends, see SUPERDIRT_OUTPUTS
		("STELLATUS-SUPERDIRT outputs = " ++ outs ++ ", orbit outs = " ++ orbitOuts).postln;

		// Added AFTER start, so it lands at the end of ~dirt.modules and runs
		// last in the chain: the resonator rings whatever the other effects
		// already did to the grain, which is the right way round — a bitcrush
		// after a resonator eats the resonance.
		//
		// Gated on `rsnpitch`, per SuperDirt's own convention that a per-event
		// module runs only when its parameter is PRESENT. rsnpitch is a MIDI
		// note, so 0 is 8.2 Hz and safely unusable — which lets the encoder use
		// zero-is-off here as it does everywhere else.
		~dirt.addModule('conspicillum_resonator', { |dirtEvent|
			var defName = if(~rsnmodel.notNil and: { ~rsnmodel > 0.5 })
				{ "dirt_rsn_pluck" } { "dirt_rsn_klank" };
			dirtEvent.sendSynth(defName ++ ~dirt.numChannels, [
				rsnpitch: ~rsnpitch,
				rsndecay: ~rsndecay ? 1.5,
				rsnbright: ~rsnbright ? 0.7,
				rsnmix: ~rsnmix ? 1.0,
				out: ~out
			])
		}, { ~rsnpitch.notNil });

		// --- the resonator that can actually ring --------------------------
		//
		// A per-orbit GlobalDirtEffect, so it lives for the life of the orbit
		// and its decay is bounded by nothing. The whole cloud excites ONE
		// resonant body — many grains striking one string, which is both far
		// cheaper than a bank per grain and the more physical of the two.
		//
		// It is a parallel SEND, like dirt_reverb: read the orbit's dry bus,
		// ring, write the effect bus. `grsn` is the send amount and behaves
		// like `room` — the chain's zero is what turns it off, because a
		// global effect only ever resumes.
		[1, 2].do { |nc|
			SynthDef("dirt_rsn_global" ++ nc, { |dryBus, effectBus, gate = 1,
					grsn = 0, grsnpitch = 45, grsndecay = 3, grsnbright = 0.85|
				var parts = 12;
				var in = In.ar(dryBus, nc).asArray.sum
					* grsn.lag(LFNoise1.kr(1).range(0.01, 0.02)) * 0.05;
				var f0 = grsnpitch.midicps;
				var snd = DynKlank.ar(`[
						(1..parts) * f0,
						(1..parts).collect { |k| 1 / k },
						(1..parts).collect { |k| grsndecay * grsnbright.pow(k - 1) }
					], in);
				snd = LeakDC.ar(snd).tanh;
				snd = snd * EnvGen.kr(Env.asr, gate, doneAction: 2);
				// Twelve ringing filters are not free to leave running, and a
				// silent orbit should cost nothing. Same grace the stock
				// globals give themselves.
				DirtPause.ar(snd, graceTime: 4);
				Out.ar(effectBus, if(nc > 1) { snd ! nc } { snd });
			}, [\ir, \ir]).add;
		};
		s.sync;

		// Inserted before dirt_monitor, which must stay last because it is the
		// output routing and the limiter. `globalEffects_` is a plain setter
		// and `initNodeTree` releases and replays the lot, so this is the
		// supported way in rather than a reach through the back.
		~dirt.orbits.do { |orbit|
			var fx = orbit.globalEffects;
			orbit.globalEffects = fx.copyRange(0, fx.size - 2)
				++ [ GlobalDirtEffect(\dirt_rsn_global,
						[\grsn, \grsnpitch, \grsndecay, \grsnbright]) ]
				++ [ fx.last ];
			orbit.initNodeTree;
		};

		SuperDirt.default = ~dirt;
		("STELLATUS-SUPERDIRT READY on port " ++ port
			++ " (+ conspicillum_resonator, dirt_rsn_global)").postln;
	};

	s.latency = 0.3;
};
