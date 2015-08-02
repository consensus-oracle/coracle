  var data = {nodes:[],
  links:[]};
  var svg;
  var paths;
  var masterNodeDrag;
  var nodesDrag;
  var start;
  var end;
$(document).ready(function () {
  svg = d3.select('svg');
  
  masterNodeDrag = d3.behavior.drag();
  d3.selectAll('.masterNode').call(masterNodeDrag);
  
  masterNodeDrag.on('dragstart',function(){
    if (mode != 'nodes'){
      return;
    }
    var tyoe;
    switch (this.id){
      case 'masterServer':
        type = 'Server';
        break;
      case 'masterHub':
        type = 'Hub';
        break;
      case 'masterClient':
        type = 'Client';
        break;
    }
    var coordinates = [0, 0];
    coordinates = d3.mouse(this);
    data.nodes.push({id:data.nodes.length+1,cx:coordinates[0],cy:coordinates[1],type:type,events:[{time:0,active:true}]});
    updateNodes();
  });
  
  masterNodeDrag.on('drag',function(){
    if (mode != 'nodes'){
        return;
      }
    console.log('dragging');
    data.nodes[data.nodes.length -1].cx = d3.event.x;
    data.nodes[data.nodes.length -1].cy = d3.event.y;
    updateNodes();
  });
  
  masterNodeDrag.on('dragend',function(){
	bringNodesIntoBounds();
  });
  
  nodesDrag = d3.behavior.drag();
  
  nodesDrag.on('drag',function(){
    d3.select(this).data()[0].cx = d3.event.x;
    d3.select(this).data()[0].cy = d3.event.y;
    updateNodes();
  });
  
  nodesDrag.on('dragend',function(){
	bringNodesIntoBounds();
  });

  
  /*var masterLLinkButton = d3.select('.masterLLinkButton');

  masterLLinkButton.on('mouseover',function(){
    d3.select(this).classed('mouseover',true);
  })
  .on('mouseout',function(){
    d3.select(this).classed('mouseover',false);
  })
  .on('click',function(){
    d3.select(this).classed('active',true);
    var nodes = svg.selectAll('.nodes');
    nodes.classed('clickable',true)
    updateNodes();
  });
  */
  
  svg.on('mousemove',function(){
    if (start != null){
      updateLinks();
    }
  });
  
  window.addEventListener('keydown',function(event){
    if (event.defaultPrevented) {
      return; // Should do nothing if the key event was already consumed.
    }
    
    if (event.keyCode == 27){
      //if Escape key
      cancelLink()
    }
  
  });

    
});

  function cancelLink(){
    if (start != null){
        start = null;
        data.links.splice(-1,1);
        updateLinks();
        d3.selectAll('.nodes').classed('clickable',mode == 'links');
      }
  }

  function updateNodes(){
    var servers = svg.selectAll('circle.server')
      .data(data.nodes.filter(serverFilter),function(d) { return d.id;})
      .call(nodesDrag);
      
    servers.attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;})
      .classed('disabled',getDisabledState);
    
    servers.enter().append('circle')
      .attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;})
      .classed('server',true)
      .classed('nodes',true);
      
    var clients = svg.selectAll('circle.client')
      .data(data.nodes.filter(clientFilter),function(d) { return d.id;})
      .call(nodesDrag);
      
    clients.attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;})
      .classed('disabled',getDisabledState);
      
    clients.enter().append('circle')
      .attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;})
      .classed('client',true)
      .classed('nodes',true);
      
    var hubs = svg.selectAll('rect.hub')
      .data(data.nodes.filter(hubFilter),function(d) { return d.id;})
      .call(nodesDrag);
      
    hubs.attr('x',getHubXCoOrd)
      .attr('y',getHubYCoOrd)
      .classed('disabled',getDisabledState);
      
    hubs.enter().append('rect')
      .attr('x',getHubXCoOrd)
      .attr('y',getHubYCoOrd)
      .attr('width',10)
      .attr('height',10)
      .classed('hub',true)
      .classed('nodes',true);
      
    var clickableNodes = d3.selectAll('.nodes.clickable');
    clickableNodes.on('click',function(){
      if (!d3.select(this).classed('clickable')){
        return;
      }
      if (start == null){
        data.links.push({start:d3.select(this).data()[0].id,id:data.links.length +1,direction:"bi"}, events:[{time:0,active:true}]);
        d3.select(this).classed('clickable',false);
        console.log(data.links);
        start = d3.select(this).data()[0].id;
        updateNodes();
        return;
      }
      data.links[data.links.length -1].end = d3.select(this).data()[0].id;
      //data.links.push({start:data.links[data.links.length -1].end,end:data.links[data.links.length -1].start,id:data.links.length +1});
      clickableNodes.classed('clickable',false);
      d3.selectAll('.nodes').classed('clickable',true);
      console.log(data.links);
      start = null;
      updateNodes();
    });
    
    var toggleables = d3.selectAll('.toggleState');
    toggleables.on('click',function(){
      console.log('toggle');
      if (!d3.select(this).classed('toggleState') || mode !='disable'){
        console.log('exiting');
        return;
      }
      var disabled = d3.select(this).classed('disabled');
      var localEvents = d3.select(this).data()[0].events;
      var event = $.grep(localEvents,function(n,i){
        return n.time == time;
      });
      if (event.length == 0){
        console.log('no event');
        localEvents.push({time:time,active:disabled});
      }
      else{
        event[0].active = disabled;
      }
      localEvents.sort(function(a,b){
        return a.time - b.time
      });
      d3.select(this).classed('disabled',!disabled);
    });
    
    updateLinks();
  }
  
  function bringNodesIntoBounds(){
	svg.selectAll('.nodes').
		data(data.nodes.filter(function(d){ return d.cx < 100}),function(d) { return d.id;})
		.transition()
		.attr('cx',100)
		.each(function (d){
			d.cx=100;
		});
		
	svg.selectAll('.nodes').
		data(data.nodes.filter(function(d){ return d.cx > 500}),function(d) { return d.id;})
		.transition()
		.attr('cx',500)
		.each(function (d){
			d.cx=500;
		});
		
	svg.selectAll('.nodes').
		data(data.nodes.filter(function(d){ return d.cy < 0}),function(d) { return d.id;})
		.transition()
		.attr('cy',0)
		.each(function (d){
			d.cy=0;
		});
		
	svg.selectAll('.nodes').
		data(data.nodes.filter(function(d){ return d.cy > 500}),function(d) { return d.id;})
		.transition()
		.attr('cy',500)
		.each(function (d){
			d.cy=500;
		});
		
	transitionHubs();
	paths
	  .transition()
	  .attr('x1',getLinkX1CoOrd)
      .attr('x2',getLinkX2CoOrd)
      .attr('y1',getLinkY1CoOrd)
      .attr('y2',getLinkY2CoOrd);
  }
  
  function transitionHubs(){
	svg.selectAll('rect.hub')
		.data(data.nodes.filter(hubFilter),function(d) { return d.id;})
		.transition()
		.attr('x',getHubXCoOrd)
		.attr('y',getHubYCoOrd);
  }
  
  function getHubXCoOrd(hub){
	return hub.cx - 5;
  }
  
  function getHubYCoOrd(hub){
	return hub.cy - 5;
  }
  
  function updateLinks(){
    paths = svg.selectAll('.link')
      .data(data.links,function(d) { return d.id;});
      
    paths.attr('x1',getLinkX1CoOrd)
      .attr('x2',getLinkX2CoOrd)
      .attr('y1',getLinkY1CoOrd)
      .attr('y2',getLinkY2CoOrd)
      .classed('disabled',getDisabledState);
      
    paths.enter().append('line')
      .attr('x1',getLinkX1CoOrd)
      .attr('x2',getLinkX2CoOrd)
      .attr('y1',getLinkY1CoOrd)
      .attr('y2',getLinkY2CoOrd)
      .classed('link',true);
      
    paths.exit().remove();
  }
  
  function getLinkX1CoOrd(d){
    return data.nodes[d.start -1].cx;
  }
  
  function getLinkX2CoOrd(d){
    if (d.end != null){
		return data.nodes[d.end -1].cx
    }
    var coordinates = [0, 0];
    coordinates = d3.mouse(this);
    return coordinates[0];
  }
  
  function getLinkY1CoOrd(d){
    if (data.nodes[d.start -1].cy != null){
      return data.nodes[d.start -1].cy;
    }
    return data.nodes[d.start -1].y;
  }
  
  function getLinkY2CoOrd(d){
    if (d.end != null){
		return data.nodes[d.end -1].cy
    }
    var coordinates = [0, 0];
    coordinates = d3.mouse(this);
    return coordinates[1];
  }
  function serverFilter(d){
    return d.type == 'Server';
  }
  
  function hubFilter(d){
    return d.type == 'Hub';
  }
  
  function clientFilter(d){
    return d.type == 'Client';
  }
  
  function getDisabledState(d){
    var events = d.events.filter(function(x){return x.time <= time});
    return !events[events.length -1].active;
  }