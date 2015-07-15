var maxNodes = 20;
var maxTermination = 100000;
$(document).ready(function () {
    $('#runSim').click(function () {
		//clear all validationErrors and results
		$('.validationError').remove();
		$('#SimResults').html('');
		if (validateSettings()){
			var oldButtonText = $('#runSim').html();
			$('#runSim').html('Running...');
			$('#runSim').attr("disabled", true)
			$.post( '/runSim',
			{
			nodes: parseInt($('#numNodes').val()),
			loss: parseFloat($('#loss').val()),
			termination: parseInt($('#termination').val())
			}, 
			function(data){
			$('#SimResults').html(data);
			$('#runSim').html(oldButtonText);
			$('#runSim').removeAttr("disabled");
			});
		}
    });
	
	function validationError(errorText){
		return "<label class='validationError'><font color='red'>" + 
			errorText +"</font></label>";
	}
	
	function validateSettings(){
		var valid = true;
		var nodes = parseInt($('#numNodes').val());
		if (isNaN(nodes) || nodes <2 || nodes > maxNodes ){
			$('#numNodes').after(validationError("Must be an integer between 2 and " + maxNodes));
			valid = false;
		}
		
		var loss = parseFloat($('#loss').val())
		if (isNaN(loss) || loss < 0 || loss > 100 ){
			$('#loss').after(validationError("Must be a number between 0 and 100"));
			valid = false;
		}
		
		var termination = parseInt($('#termination').val())
		if (isNaN(termination) || termination < 1 || termination > maxTermination ){
			$('#loss').after(validationError("Must be an integer between 1 and " + maxTermination));
			valid = false;
		}
		return valid;
	}
});