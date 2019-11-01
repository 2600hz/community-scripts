// Written by Bret Truchan for 2600Hz

var DTMF = (function() {

    var key_data = {
        "1": {
            "playing": false,
            "low_dtmf_frequency": 697,
            "high_dtmf_frequency": 1209,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "2": {
            "playing": false,
            "low_dtmf_frequency": 697,
            "high_dtmf_frequency": 1336,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "3": {
            "playing": false,
            "low_dtmf_frequency": 697,
            "high_dtmf_frequency": 1447,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "4": {
            "playing": false,
            "low_dtmf_frequency": 770,
            "high_dtmf_frequency": 1209,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "5": {
            "playing": false,
            "low_dtmf_frequency": 770,
            "high_dtmf_frequency": 1336,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "6": {
            "playing": false,
            "low_dtmf_frequency": 770,
            "high_dtmf_frequency": 1477,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "7": {
            "playing": false,
            "low_dtmf_frequency": 852,
            "high_dtmf_frequency": 1209,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "8": {
            "playing": false,
            "low_dtmf_frequency": 852,
            "high_dtmf_frequency": 1336,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "9": {
            "playing": false,
            "low_dtmf_frequency": 852,
            "high_dtmf_frequency": 1477,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "*": {
            "playing": false,
            "low_dtmf_frequency": 941,
            "high_dtmf_frequency": 1209,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "0": {
            "playing": false,
            "low_dtmf_frequency": 941,
            "high_dtmf_frequency": 1336,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        },
        "#": {
            "playing": false,
            "low_dtmf_frequency": 941,
            "high_dtmf_frequency": 1477,
            "low_dtmf_audio_buffer": null,
            "high_dtmf_audio_buffer": null,
            "low_dtmf_buffer_source": null,
            "high_dtmf_buffer_source": null
        }
    };

    var audio_context = null;

    return {

        initialize: function() {

            // Fire up the browser Audio stuff
            window.AudioContext = window.AudioContext||window.webkitAudioContext;
            audio_context = new AudioContext();

            var sample_rate = audio_context.sampleRate;
            var duration_in_samples;
            var channel_data;

            // You might consider using multiple channels for the different low/high
            // dtmf tones, but it turns out that channels are interpreted as stereo
            // left-right, so the tones would be in stereo, which we don't want.
            var number_of_channels = 1;

            //
            // For each of the keys defined at the beginning of this script, create
            // two audio buffers.  Fill one with the low frequency dtmf sinewave and the
            // other with the low dtmf sinewave.  These are pre-computed for efficiency.

            for (var key in key_data) {

                //
                // Fill in the audio buffer holding the low-frequency part of the dtmf
                //

                frequency = key_data[key].low_dtmf_frequency;
                duration_in_samples = sample_rate / frequency;
                key_data[key].low_dtmf_audio_buffer = audio_context.createBuffer(number_of_channels, duration_in_samples, sample_rate);
                channel_data = key_data[key].low_dtmf_audio_buffer.getChannelData(0);

                for (var i = 0; i < duration_in_samples; i++) {
                    channel_data[i] = Math.sin(2 * Math.PI * frequency * i / (sample_rate));
                }

                //
                // Now do the same for the high-frequency part of the DFTM
                //

                frequency = key_data[key].high_dtmf_frequency;
                duration_in_samples = sample_rate / frequency;
                key_data[key].high_dtmf_audio_buffer = audio_context.createBuffer(number_of_channels, duration_in_samples, sample_rate);
                channel_data = key_data[key].high_dtmf_audio_buffer.getChannelData(0);

                for (var i = 0; i < duration_in_samples; i++) {
                    channel_data[i] = Math.sin(2 * Math.PI * frequency * i / (sample_rate));
                }
            }
        },

        playKey: function(key) {

            if(key in key_data) // Test to ensure that the key is supported
            {
                if(key_data[key].playing == false)
                {
                    key_data[key].playing = true;

                    // =========================================================================
                    // Play low tone
                    // =========================================================================
                    var low_dtmf_buffer_source = audio_context.createBufferSource();
                    low_dtmf_buffer_source.buffer = key_data[key].low_dtmf_audio_buffer;
                    low_dtmf_buffer_source.connect(audio_context.destination);
                    low_dtmf_buffer_source.loop = true;
                    low_dtmf_buffer_source.start(0); // 0 is the channel

                    // Store buffer source so that we can stop playback based on key number
                    key_data[key].low_dtmf_buffer_source = low_dtmf_buffer_source;

                    // =========================================================================
                    // play high tone
                    // =========================================================================
                    var high_dtmf_buffer_source = audio_context.createBufferSource();
                    high_dtmf_buffer_source.buffer = key_data[key].high_dtmf_audio_buffer;
                    high_dtmf_buffer_source.connect(audio_context.destination);
                    high_dtmf_buffer_source.loop = true;
                    high_dtmf_buffer_source.start(0); // 0 is the channel

                    // Store buffer source so that we can stop playback based on key number
                    key_data[key].high_dtmf_buffer_source = high_dtmf_buffer_source;
                }
            }
            else
            {
                console.warn("DTMF.js: Unable to play unsupported touch-tone key '" + key + "'");
            }
        },

        stopKey: function(key) {

            if(key in key_data) // Test to ensure that the key is supported
            {
                if(key_data[key].low_dtmf_buffer_source != null) key_data[key].low_dtmf_buffer_source.stop();
                if(key_data[key].high_dtmf_buffer_source != null) key_data[key].high_dtmf_buffer_source.stop();

                key_data[key].playing = false;
            }
            else
            {
                console.warn("DTMF.js: Unable to stop unsupported touch-tone key '" + key + "'");
            }
        },

        stopAll: function() {
            Object.keys(key_data).forEach(function(key) {
                DTMF.stopKey(key);
            })
        }
    };
})();

DTMF.initialize();
