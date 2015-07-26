$(document).ready(function () {
  var svg = d3.select('svg');
  var data = [];
  var links = [];
  
  var masterNodeDrag = d3.behavior.drag();
  d3.select('.masterNode').call(masterNodeDrag);
  
  masterNodeDrag.on('dragstart',function(){
    console.log("drag started");
    data.push({id:data.length+1,cx:0,cy:0,type:'Node'});
    console.log(d3.event);
  });
  
  masterNodeDrag.on('drag',function(){
    console.log('dragging');
    //console.log(d3.event);
    data[data.length -1].cx = d3.event.x;
    data[data.length -1].cy = d3.event.y;
    //console.log(d3.event);
    updateNodes();
  });
  
  masterNodeDrag.on('dragend',function(){
    console.log('drag ended');
  });
  
  var nodeDrag = d3.behavior.drag();
  d3.selectAll('.node').call(nodeDrag);
  
  nodeDrag.on('drag',function(){
    d3.select(this).data()[0].cx = d3.event.x;
    d3.select(this).data()[0].cy = d3.event.y;
    updateNodes();
  });
  
  var masterConnectorDrag = d3.behavior.drag();
  d3.select('.masterConnector').call(masterConnectorDrag);
  
  masterConnectorDrag.on('dragstart',function(){
    console.log("drag started");
    data.push({id:data.length+1,x:0,y:0,type:'Connector'});
    console.log(d3.event);
  });
  
  masterConnectorDrag.on('drag',function(){
    console.log('dragging');
    //console.log(d3.event);
    data[data.length -1].x = d3.event.x;
    data[data.length -1].y = d3.event.y;
    //console.log(d3.event);
    updateNodes();
  });
  
  masterConnectorDrag.on('dragend',function(){
    console.log('drag ended');
  });
  
  var connectorDrag = d3.behavior.drag();
  d3.selectAll('.connector').call(connectorDrag);
  
  connectorDrag.on('drag',function(){
    d3.select(this).data()[0].x = d3.event.x;
    d3.select(this).data()[0].y = d3.event.y;
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
  
  

  
  
  function updateNodes(){
    var nodes = svg.selectAll('circle.node')
      .data(data.filter(nodeFilter),function(d) { return d.id;})
      .call(nodeDrag);
      
    nodes.attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;});
      
    nodes.enter().append('circle')
      .attr('cx',function(d){return d.cx;})
      .attr('cy',function(d){return d.cy;})
      .classed('node',true)
      .classed('nodes',true);
      
    var connectors = svg.selectAll('rect.connector')
      .data(data.filter(connectorFilter),function(d) { return d.id;})
      .call(connectorDrag);
      
    connectors.attr('x',function(d){return d.x;})
      .attr('y',function(d){return d.y;});
      
    connectors.enter().append('rect')
      .attr('x',function(d){return d.x;})
      .attr('y',function(d){return d.y;})
      .attr('width',10)
      .attr('height',10)
      .classed('connector',true)
      .classed('nodes',true);
      
    var clickableNodes = d3.selectAll('.nodes.clickable');
    clickableNodes.on('click',function(){
      if (!d3.select(this).classed('clickable')){
        return;
      }
      if (start == null){
        links.push({start:d3.select(this).data()[0].id,id:links.length});
        d3.select(this).classed('clickable',false);
        console.log(links);
        start = d3.select(this).data()[0].id;
        updateNodes();
        return;
      }
      links[links.length -1].end = d3.select(this).data()[0].id;
      clickableNodes.classed('clickable',false);
      console.log(links);
      start = null;
      updateNodes();
    });
    
    updateLinks();
  }
  
  function updateLinks(){
    var paths = svg.selectAll('.link')
      .data(links,function(d) { return d.id;});
      
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
  
  function nodeFilter(d){
    return d.type == 'Node';
  }
  
  function connectorFilter(d){
    return d.type == 'Connector';
  }
  
  function getLinkX1CoOrd(d){
    if (data[d.start -1].cx != null){
      return data[d.start -1].cx;
    }
    return data[d.start -1].x;
  }
  
  function getLinkX2CoOrd(d){
    if (d.end != null){
      if (data[d.end -1].cx != null){
        return data[d.end -1].cx
      }
      else{
        return data[d.end -1].x
      }
    }
    var coordinates = [0, 0];
    coordinates = d3.mouse(this);
    return coordinates[0];
  }
  
  function getLinkY1CoOrd(d){
    if (data[d.start -1].cy != null){
      return data[d.start -1].cy;
    }
    return data[d.start -1].y;
  }
  
  function getLinkY2CoOrd(d){
    if (d.end != null){
      if (data[d.end -1].cy != null){
        return data[d.end -1].cy
      }
      else{
        return data[d.end -1].y
      }
    }
    var coordinates = [0, 0];
    coordinates = d3.mouse(this);
    return coordinates[1];
  }
    
});