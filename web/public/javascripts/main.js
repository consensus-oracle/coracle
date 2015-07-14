$(document).ready(function () {
    $('#runSim').click(function () {
        $.post( '/runSim',
		{
		nodes: parseInt($('#numNodes').val()),
		loss: parseFloat($('#loss').val())
		}, 
		function(data){
        $('#SimResults').html(data);
        });
    });
});