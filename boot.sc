SuperDirt.start;

MIDIClient.init;

~midiOut = MIDIOut.newByName("IAC Driver", "IACBus1");
~dirt.soundLibrary.addMIDI(\midi1, ~midiOut);
~midiOut.latency = 0.48;
