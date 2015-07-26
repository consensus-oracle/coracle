$(document).ready(function () {
  var svg = d3.select('svg');
  var data = [];
  
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
    d3.select(this).attr('cx',d3.event.x);
    d3.select(this).attr('cy',d3.event.y);
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
    d3.select(this).attr('x',d3.event.x);
    d3.select(this).attr('y',d3.event.y);
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
      .classed('node',true);
      
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
      .classed('connector',true);
  }
  
  function nodeFilter(d){
    return d.type == 'Node';
  }
  
  function connectorFilter(d){
    return d.type == 'Connector';
  }
    
});