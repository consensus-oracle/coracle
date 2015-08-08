var maxNodes = 20;
var maxTermination = 100000;
var mode= 'nodes';
var time=0;
$(document).ready(function () {
  $(".btn-group > .btn").click(function(){
    $(this).addClass("active").siblings().removeClass("active");
    switch($(this).attr('id')){
      case 'nodesModeRadioButton':
        mode= 'nodes';
        break;
      case 'linksModeRadioButton':
        mode= 'links';
        break;
      case 'disableModeRadioButton':
        mode= 'disable';
        break;
    }
    //console.log('mode: ' + mode);
    d3.selectAll('.masterNode').classed('disabled',mode != 'nodes');
    d3.selectAll('.nodes').classed('clickable',mode == 'links')
      .classed('toggleState',mode == 'disable');
    paths.classed('toggleState',mode == 'disable');
    if (mode != 'links'){
      cancelLink()
    }
    updateNodes();
});

  $.get( '/examples.json',function(response){
    console.log(response);
    d3.select('#carouselIndicators').selectAll('li').data(response.templateNames)
      .enter()
      .append('li')
      .attr('data-target','#examplesCarousel')
      .attr('data-slide-to',function(d,i){return i-1;})
      .classed('active',function(d,i){return i-1 ==0;})
      .classed('carouselIndicator',true);
      
    var items =d3.select('#carouselItems').selectAll('div.item').data(response.templateNames)
      .enter()
      .append('div')
        .classed('item',true)
        .classed('active',function(d,i){return i-1 ==0;})
        .on('click',function(d){
          $.get('/examples/' + d + '.json', function(response){
            data = response;
            updateNodes();
          });
        });
    
    items.append('img')
        .attr('src',function(d){return '/examples/' + d + '.png';});
    items.append('div')
      .classed('carousel-caption',true)
      .html(function(d){return d.replace('_',' ');});
  });

  $('#termination').on( 'change', function () {
    //update max for time slider to be termination
    $slider = $('#timeSlider');
    var max =parseInt($('#termination').val());
    var value = Math.min($slider.data('slider').getValue(),max);
    $slider.slider('setAttribute', 'max', max);
    $slider.slider('setValue', value);
    $('#timeSliderMax').text(max);
    if (value != time){
    //TODO: do we need to remove events that are now past termination time or are they ignored?
      time = value;
      $('#currentTime').text(value);
      updateNodes();
    }
  });

  $("#lossSlider").slider({
  });
  
  $("#timeSlider").slider({});
  $('#timeSlider').on('slide',function(slideEvt){
    time = slideEvt.value;
    $('#currentTime').text(time);
    updateNodes();
  });
  
    $('#runSim').click(function () {
		//clear all validationErrors and results
		$('.validationError').remove();
    $('.has-error').removeClass('has-error');
		$('#resultsPanel').addClass('hidden');
    $('#tracePanel').addClass('hidden');
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
					result = validationError("Failed: " + response.stderr)
          result += validationError('Stdout: ' + response.stdout);
				}
				else{
          try{
            var jsonResults = JSON.parse(response.stdout);
            result = JSON.stringify(jsonResults.results);
            createTable(jsonResults);
          }
          catch(err){
            result = validationError('Parsing error: ' + err);
            result += validationError('Full Response: ' + JSON.stringify(response));
          }          
          $('#tracePanel').removeClass('hidden');
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
    $('#TraceResults').html('\
    <table id="resultsTables" class="display" cellspacing="0" width="100%">\
        <thead>\
            <tr>\
                <th>Time</th>\
                <th>Node ID</th>\
                <th>Type</th>\
                <th>Data</th>\
            </tr>\
        </thead>\
 \
        <tfoot>\
            <tr>\
                <th>Time</th>\
                <th>Node ID</th>\
                <th>Type</th>\
                <th>Data</th>\
            </tr>\
        </tfoot>\
    </table>');
    console.log(jsonObject);
    console.log('trace: ');
    console.log(jsonObject.trace);
    $('#resultsTables').DataTable({
      data: jsonObject.trace,
      columns:  [
         {data: 'time'},
         {data: 'id'},
         {data: 'event.type'},
         {data: 'event.data',
          'render': function( data, type, full, meta){
            if (data == null){
              return '';
            }
            else{
              return JSON.stringify(data);
            }
          }}
      ],
      initComplete: onInitComplete
    });
	}
  
  function onInitComplete(){
    this.api().columns().every( function () {
      var column = this;
      console.log(column);
      //for now don't do data column
      if (column.index() == 3){
        return;
      }
      var select = $('<select><option value=""></option></select>')
          .appendTo( $(column.footer()).empty() )
          .on( 'change', function () {
              var val = $.fn.dataTable.util.escapeRegex(
                  $(this).val()
              );

              column
                  .search( val ? '^'+val+'$' : '', true, false )
                  .draw();
          } );

      column.data().unique().sort().each( function ( d, j ) {
          select.append( '<option value="'+d+'">'+d+'</option>' )
      } );
    } );
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
      seed: parseInt($('#randomSeed').val()),
      workload_min: parseInt($('#workload_min').val()),
      workload_max: parseInt($('#workload_max').val()),
      consensus: {
        protocol:"raft",
        election_timeout_min: parseInt($('#election_min').val()), 
        election_timeout_max: parseInt($('#election_max').val()),
        client_timeout: parseInt($('#client').val()),
        heartbeat_interval: parseInt($('#heartbeat').val())
      },
      network:{
        nodes:data.nodes,
        links:data.links,
        events:[]
      }
    };
    
    data.nodes.forEach(function(node){
      //console.log(node);
      node.events.forEach(function(nodeEvent){
        var event = $.grep(result.network.events,function(n,i){
          return n.time == nodeEvent.time;
        });
        
        if (event.length == 0){
          result.network.events.push({time:nodeEvent.time,links:[],nodes:[]});
          event = $.grep(result.network.events,function(n,i){
            return n.time == nodeEvent.time;
          });
        }
        console.log(event)
        event[0].nodes.push({id:node.id,active:nodeEvent.active});
      });
    });
    
    data.links.forEach(function(link){
      link.events.forEach(function(linkEvent){
        var event = $.grep(result.network.events,function(n,i){
          return n.time == linkEvent.time;
        });
        
        if (event.length == 0){
          result.network.events.push({time:linkEvent.time,links:[],nodes:[]});
          event = $.grep(result.network.events,function(n,i){
            return n.time == linkEvent.time;
          });
        }
        console.log(linkEvent);
        event[0].links.push({id:link.id,active:linkEvent.active,type:'s'});
        
      });
    });
    
    
    var string = JSON.stringify(result,null,'\t');
    console.log(result);
    return string;
  }
  
});
