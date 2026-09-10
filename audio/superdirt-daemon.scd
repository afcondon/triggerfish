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
// --- swept sample sets (WORKSHOP_SETS_DIR) -----------------------------------
// The Workshop cuts a take into a SET — a folder of numbered WAVs plus a
// `set.json` saying what each one meant — under ~/.itajara/workshop/samples.
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
var setsDir = "WORKSHOP_SETS_DIR".getenv ? ("HOME".getenv +/+ ".itajara/workshop/samples");
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
	s.options.numOutputBusChannels = 2;
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
		~dirt.start(port, 0 ! 12);        // 12 orbits, all to channel 0
		SuperDirt.default = ~dirt;
		("STELLATUS-SUPERDIRT READY on port " ++ port).postln;
	};

	s.latency = 0.3;
};
