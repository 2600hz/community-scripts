$(document).ready(function() {

    var playing_key = null;

    $('button').mousedown(function() {
        var key = $(this).data('key');
        DTMF.playKey(key);
        playing_key = key;
    });

    $(document).mouseup(function() {
        if(playing_key != null)
        {
            DTMF.stopKey(playing_key);
            playing_key = null;
        }
    });
});
