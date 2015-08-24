function createGraph1(chartData){
  var svg = d3.select('#graphs').append('svg').attr('width',1000).attr('height',300);
  var chart = nv.models.lineChart()
                .margin({left: 100})  //Adjust chart margins to give the x-axis some breathing room.
                .useInteractiveGuideline(true)  //We want nice looking tooltips and a guideline!
                .showLegend(true)       //Show the legend, allowing users to turn on/off line series.
                .showYAxis(true)        //Show the y-axis
                .showXAxis(true)        //Show the x-axis
                .width(1000)
                .height(300)
                .xDomain([chartData['x axis'].start,chartData['x axis'].end])
                .yDomain([chartData['y axis'].start,chartData['y axis'].end])
  ;
  chart.xAxis     //Chart x-axis settings
      .axisLabel(chartData['x axis'].label)
      .ticks(10);
  
  chart.yAxis     //Chart y-axis settings
      .axisLabel(chartData['y axis'].label)
      .tickFormat(d3.format("d"));
      
  console.log(chartData);
  
  var lineData = chartData.data.map(function (d){return {values:d.data, key:d['line id']}}).sort(function(a,b){return a.key -b.key});
  console.log(lineData);
  svg
    .datum(lineData)
    .call(chart);
    
  //Update the chart when window resizes.
  nv.utils.windowResize(function() { chart.update() });
  return chart;
}
