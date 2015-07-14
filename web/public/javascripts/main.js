$(document).ready(function () {
    $('#runSim').click(function () {
        $.get( '/runSim', function(data){
        $('#SimResults').html(data);
        });
    });
});