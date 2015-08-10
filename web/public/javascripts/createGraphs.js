function createGraph1(chartData){
  var chart = nv.models.lineChart()
                .margin({left: 100})  //Adjust chart margins to give the x-axis some breathing room.
                .useInteractiveGuideline(true)  //We want nice looking tooltips and a guideline!
                .transitionDuration(350)  //how fast do you want the lines to transition?
                .showLegend(true)       //Show the legend, allowing users to turn on/off line series.
                .showYAxis(true)        //Show the y-axis
                .showXAxis(true)        //Show the x-axis
  ;
  chart.xAxis     //Chart x-axis settings
      .axisLabel('Time (ms)')
      .tickFormat(d3.format(',r'));
      
  chart.yAxis     //Chart y-axis settings
      .axisLabel('Voltage (v)')
      .tickFormat(d3.format('.02f'));
      
  d3.select('#Graph1 svg')
    .datum(chartData)
    .call(chart);
}