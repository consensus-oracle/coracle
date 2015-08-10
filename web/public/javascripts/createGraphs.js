function createGraph1(chartData){
  var chart = nv.models.lineChart()
                .margin({left: 100})  //Adjust chart margins to give the x-axis some breathing room.
                .useInteractiveGuideline(true)  //We want nice looking tooltips and a guideline!
                .showLegend(true)       //Show the legend, allowing users to turn on/off line series.
                .showYAxis(true)        //Show the y-axis
                .showXAxis(true)        //Show the x-axis
                .width(500)
                .height(500)
  ;
  chart.xAxis     //Chart x-axis settings
      .axisLabel('Time (ms)');
      
  chart.yAxis     //Chart y-axis settings
      .axisLabel('Voltage (v)');
      
  console.log(chartData);
  d3.select('#Graph1')
    .datum([{values:chartData,
             key: 'line 1'}])
    .call(chart);
    
  //Update the chart when window resizes.
  nv.utils.windowResize(function() { chart.update() });
  return chart;
}