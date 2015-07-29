  var data = {nodes:[],
  links:[]};
  
$(document).ready(function () {
  var svg = d3.select('svg');
  
  var masterServerDrag = d3.behavior.drag();
  d3.select('.masterServer').call(masterServerDrag);
  
  masterServerDrag.on('dragstart',function(){
    console.log("drag started");
    data.nodes.push({id:data.nodes.length+1,cx:0,cy:0,type:'Server'});
    console.log(d3.event);
  });
  
  masterServerDrag.on('drag',function(){
    console.log('dragging');
    data.nodes[data.nodes.length -1].cx = d3.event.x;
    data.nodes[data.nodes.length -1].cy = d3.event.y;
    updateNodes();
  });
  
  masterServerDrag.on('dragend',function(){
    console.log('drag ended');
  });
  
  var serverDrag = d3.behavior.drag();
  d3.selectAll('.server').call(serverDrag);
  
  serverDrag.on('drag',function(){
    d3.select(this).data()[0].cx = d3.event.x;
    d3.select(this).data()[0].cy = d3.event.y;
    updateNodes();
  });
  
  var masterHubDrag = d3.behavior.drag();
  d3.select('.masterHub').call(masterHubDrag);
  
  masterHubDrag.on('dragstart',function(){
    console.log("drag started");
    data.nodes.push({id:data.nodes.length+1,cx:0,cy:0,type:'Hub'});
    console.log(d3.event);
  });
  
  masterHubDrag.on('drag',function(){
    console.log('dragging');
    data.nodes[data.nodes.length -1].cx = d3.event.x;
    data.nodes[data.nodes.length -1].cy = d3.event.y;
    updateNodes();
  });
  
  masterHubDrag.on('dragend',function(){
    console.log('drag ended');
  });
  
  var hubDrag = d3.behavior.drag();
  d3.selectAll('.hub').call(hubDrag);
  
  hubDrag.on('drag',function(){
    d3.select(this).data()[0].cx = d3.event.x;
    d3.select(this).data()[0].cy = d3.event.y;
    updateNodes();
  });
  
  var masterLLinkButton = d3.select('.masterLLinkButton');
  var start;
  var end;
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
  
  svg.on('mousemove',function(){
	if (start != null){
		updateLinks();
	}
  });
  
  

  
  
  function updateNodes(){
    var servers = svg.selectAll('circle.server')
      .data(data.nodes.filter(serverFilter),function(d) { return d.id;})
      .call(serverDrag);
      
    servers.attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;});
      
    servers.enter().append('circle')
      .attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;})
      .classed('server',true)
      .classed('nodes',true);
      
    var hubs = svg.selectAll('rect.hub')
      .data(data.nodes.filter(hubFilter),function(d) { return d.id;})
      .call(hubDrag);
      
    hubs.attr('x',getHubXCoOrd)
      .attr('y',getHubYCoOrd);
      
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
        data.links.push({start:d3.select(this).data()[0].id,id:data.links.length +1});
        d3.select(this).classed('clickable',false);
        console.log(data.links);
        start = d3.select(this).data()[0].id;
        updateNodes();
        return;
      }
      data.links[data.links.length -1].end = d3.select(this).data()[0].id;
      data.links.push({start:data.links[data.links.length -1].end,end:data.links[data.links.length -1].start,id:data.links.length +1});
      clickableNodes.classed('clickable',false);
      console.log(data.links);
      start = null;
      updateNodes();
    });
    
    updateLinks();
  }
  
  function getHubXCoOrd(hub){
	return hub.cx - 5;
  }
  
  function getHubYCoOrd(hub){
	return hub.cy - 5;
  }
  
  function updateLinks(){
    var paths = svg.selectAll('.link')
      .data(data.links,function(d) { return d.id;});
      
    paths.attr('x1',getLinkX1CoOrd)
      .attr('x2',getLinkX2CoOrd)
      .attr('y1',getLinkY1CoOrd)
      .attr('y2',getLinkY2CoOrd);
      
    paths.enter().append('line')
      .attr('x1',getLinkX1CoOrd)
      .attr('x2',getLinkX2CoOrd)
      .attr('y1',getLinkY1CoOrd)
      .attr('y2',getLinkY2CoOrd)
      .classed('link',true);
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
    
});

  function serverFilter(d){
    return d.type == 'Server';
  }
  
  function hubFilter(d){
    return d.type == 'Hub';
  }