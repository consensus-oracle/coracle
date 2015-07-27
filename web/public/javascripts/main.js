var maxNodes = 20;
var maxTermination = 100000;
$(document).ready(function () {
  $("#lossSlider").slider({
  });
    $('#runSim').click(function () {
		//clear all validationErrors and results
		$('.validationError').remove();
    $('.has-error').removeClass('has-error');
		$('#resultsPanel').addClass('hidden');
		if (validateSettings()){
			var oldButtonText = $('#runSim').html();
			$('#runSim').html('Running...');
			$('#runSim').attr("disabled", true)
			$.post( '/runSim',
			{data:generateJSON()}, 
			function(response){
				var result;
        //response.stdout ='{"packets dispatched":8,"packets received":8,"packets dropped":0}';
        //response.error = null;
				if (response.error != null){
					console.log(response);
					result = validationError("Failed: " + response.stderr);
				}
				else{
					result = response.stdout; //createTable(JSON.parse(response.stdout));
				}
        $('#resultsPanel').removeClass('hidden');
        $('#resultsTab').tab('show');
				$('#SimResults').html(result);
				$('#runSim').html(oldButtonText);
				$('#runSim').removeAttr("disabled");
			});
		}
    });
	
	function validationError(errorText){
		return '<div class="alert alert-danger validationError" role="alert">'
      +'<span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>'
      +'<span class="sr-only">Error:</span>'
  + errorText +
  '</div>';
	}
	
	function validateSettings(){
		var valid = true;
    /*
		if (!checkBounds(parseInt($('#numNodes').val()),2,maxNodes)){
			$('#nodesDiv').after(validationError("Must be an integer between 2 and " + maxNodes));
      $('#nodesDiv').addClass('has-error');
			valid = false;
		}
		
		if (!checkBounds(parseFloat($("#lossSlider").val()),0,1)){
			$('#lossDiv').after(validationError("Must be a number between 0 and 100"));
      $('#lossDiv').addClass('has-error');
			valid = false;
		}
		*/
		if (!checkBounds(parseInt($('#termination').val()),1,maxTermination)){
			$('#terminationDiv').after(validationError("Must be an integer between 1 and " + maxTermination));
      $('#terminationDiv').addClass('has-error');
			valid = false;
		}
		return valid;
	}
	
	function checkBounds(value, min, max){
		return (!isNaN(value) && value >= min && value <=max);
	}
	
	function createTable(jsonObject){
		//start of html for the table
		var table = "<table class='table table-bordered' id='results'>\
			<tr><thead>";
		//add in the headers from the JSON variables
		for (var key in jsonObject){
			table += "<th>" + key + "</th>";
		}
		table += "</tr></thead><tr>";
		
		//now add in the values, assumes no list values
		for (var key in jsonObject){
			table += "<td>" + jsonObject[key] + "</td>";
		}
		table += "</tr></table>";
		
		return table;
	}
  
  function generateJSON(){
    /*
      return 			{
			nodes: parseInt($('#numNodes').val()),
			loss: parseFloat($("#lossSlider").val()),
			termination: parseInt($('#termination').val())
			};
    */
    
    var result = {
      termination: parseInt($('#termination').val()),
      consensus: {
        protocol:"raft",
        election_timeout_min:30, 
        election_timeout_max:300, 
        heartbeat_interval:30
      },
      network:{
        nodes:data.nodes,
        links:data.links,
        events:[{
          time:0,
          links:[],
          nodes:[]
          }
        ]
      }
    };
    
    data.nodes.filter(serverFilter).forEach(function(node){
      //console.log(node);
      result.network.events[0].nodes.push({id:node.id,active:true});
    });
    
    data.links.forEach(function(link){
      //console.log(link);
      result.network.events[0].links.push({id:link.id,active:true,type:'s'});
    });
    var string = JSON.stringify(result,null,'\t');
    console.log(result);
    return string;
  }
  
});