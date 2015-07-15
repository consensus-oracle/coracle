$(document).ready(function () {
    $('#runSim').click(function () {
        $.post( '/runSim',
		{
		nodes: parseInt($('#numNodes').val()),
		loss: parseFloat($('#loss').val()),
		termination: parseInt($('#termination').val())
		}, 
		function(data){
        $('#SimResults').html(data);
        });
    });
});