//
// Simple demo using the DTMF.js library
//

$dtmf_debug = true;

$(document).ready(function(){

    $(document).keydown(function(e) {

        if ($(".phone-number-input").is(":focus")) {

            if($dtmf_debug) console.log("You pressed " + e.which);

            switch(e.which) {
                case 97: case 49:
                    DTMF.playKey("1");
                    break;
                case 98: case 50:
                    DTMF.playKey("2");
                    break;
                case 99: case 51:
                    DTMF.playKey("3");
                    break;
                case 100: case 52:
                    DTMF.playKey("4");
                    break;
                case 101: case 53:
                    DTMF.playKey("5");
                    break;
                case 102: case 54:
                    DTMF.playKey("6");
                    break;
                case 103: case 55:
                    DTMF.playKey("7");
                    break;
                case 104: case 56:
                    DTMF.playKey("8");
                    break;
                case 105: case 57:
                    DTMF.playKey("9");
                    break;
                case 96: case 48:
                    DTMF.playKey("0");
                    break;
                case 35:
                    DTMF.playKey("#");
                    break;
                case 42:
                    DTMF.playKey("*");
                    break;
            } // end switch statement
        } // end if input has focus
    }); // end keydown event handler

    $(document).keyup(function(e) {

        if ($(".phone-number-input").is(":focus")) {

            if($dtmf_debug) console.log("You released " + e.which);

            switch(e.which) {
                case 97: case 49:
                        DTMF.stopKey("1");
                    break;
                case 98: case 50:
                    DTMF.stopKey("2");
                    break;
                case 99: case 51:
                    DTMF.stopKey("3");
                    DTMF.stopKey("#");
                    break;
                case 100: case 52:
                    DTMF.stopKey("4");
                    break;
                case 101: case 53:
                    DTMF.stopKey("5");
                    break;
                case 102: case 54:
                    DTMF.stopKey("6");
                    break;
                case 103: case 55:
                    DTMF.stopKey("7");
                    break;
                case 104: case 56:
                    DTMF.stopKey("8");
                    DTMF.stopKey("*");
                    break;
                case 105: case 57:
                    DTMF.stopKey("9");
                    break;
                case 96: case 48:
                    DTMF.stopKey("0");
                    break;
            } // end switch statement
        } // end if input has focus
    }); // end keyup event handler

    // Stop all tones if the input loses focus
    $('.phone-number-input').on("blur", function(){
        if($dtmf_debug) console.log("Input lost focus.  Stopping all tones.");
        DTMF.stopAll();
    });

}); // end document ready handler
