//
// Simple demo using the DTMF.js library
//

$(document).keypress(function(e) {
    switch(e.which) {
        case 49:
            DTMF.playKey("1");
            break;
        case 50:
            DTMF.playKey("2");
            break;
        case 51:
            DTMF.playKey("3");
            break;
        case 52:
            DTMF.playKey("4");
            break;
        case 53:
            DTMF.playKey("5");
            break;
        case 54:
            DTMF.playKey("6");
            break;
        case 55:
            DTMF.playKey("7");
            break;
        case 56:
            DTMF.playKey("8");
            break;
        case 57:
            DTMF.playKey("9");
            break;
        case 48:
            DTMF.playKey("0");
            break;
        case 35:
            DTMF.playKey("#");
            break;
        case 42:
            DTMF.playKey("*");
            break;
    }
});

$(document).keyup(function(e) {
    switch(e.which) {
        case 49:
            DTMF.stopKey("1");
            break;
        case 50:
            DTMF.stopKey("2");
            break;
        case 51:
            DTMF.stopKey("3");
            DTMF.stopKey("#");
            break;
        case 52:
            DTMF.stopKey("4");
            break;
        case 53:
            DTMF.stopKey("5");
            break;
        case 54:
            DTMF.stopKey("6");
            break;
        case 55:
            DTMF.stopKey("7");
            break;
        case 56:
            DTMF.stopKey("8");
            DTMF.stopKey("*");
            break;
        case 57:
            DTMF.stopKey("9");
            break;
        case 48:
            DTMF.stopKey("0");
            break;
    }
});
