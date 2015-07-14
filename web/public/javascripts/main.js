$(document).ready(function () {
    $('#runSim').click(function () {
        $.post( '/runSim',
		{
		nodes: $('#numNodes').val(),
		loss: $('#loss').val()
		}, 
		function(data){
        $('#SimResults').html(data);
        });
    });
});