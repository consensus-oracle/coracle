var runConfigs = [];
var editing = false;
function addRunConfig(){
    editing=false;
    $('#networkSettingsModal').modal('show');
    $('#settingsModalTab').tab('show');
}

function removeConfig(){
  updateRunConfigList()
  var activeItem = d3.select('#runSettingsList').select('a.list-group-item.active');
  console.log(activeItem.data());
  console.log(runConfigs.indexOf(activeItem.data()[0]));
  runConfigs.splice(runConfigs.indexOf(activeItem.data()[0]),1);
  updateRunConfigList();
}

function editConfig(){
  updateRunConfigList()
  var activeItem = d3.select('#runSettingsList').select('a.list-group-item.active');
  
  var settings = activeItem.datum();
  $('#runConfigName').val(settings.name);
  $('#termination').val(settings.termination);
  $('#randomSeed').val(settings.seed);
  $('#workload_min').val(settings.workload_min);
  $('#workload_max').val(settings.workload_max);
  $('#election_min').val(settings.consensus.election_timeout_min);
  $('#election_max').val(settings.consensus.election_timeout_max);
  $('#client').val(settings.consensus.client_timeout);
  $('#heartbeat').val(settings.consensus.heartbeat_interval);
  console.log('setting data:');
  console.log(settings.data);
  data = settings.data;
  updateNodes();
  $('#networkSettingsModal').modal('show');
  editing=true;
}

function saveConfig(){

  var newRunConfig ={
      name:$('#runConfigName').val(),
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
      data: JSON.parse(JSON.stringify(data))
        
    }

  if (editing){
    console.log('editing');
    updateRunConfigList()
    var activeItem = d3.select('#runSettingsList').select('a.list-group-item.active');
    var settings = activeItem.datum();
    settings = newRunConfig;
  }
  else{
    runConfigs.push(newRunConfig);
  }
  $('#SettingsNextButton').removeClass('hidden');
  $('#SettingsSaveButton').addClass('hidden');
  $('#settingsModalTab').tab('show');
  $('#networkSettingsModal').modal('hide');
  console.log(runConfigs);
  updateRunConfigList();
  var editing=false;
}

function onRunConfigClick(listItem){
  //console.log(listItem);
  $(this).addClass("active").siblings().removeClass("active");

}

function updateRunConfigList(){

  var runconfigListItems = d3.select('#runSettingsList').selectAll('a.list-group-item').data(runConfigs);
  
  runconfigListItems.enter()
    .append('a')
      .classed('list-group-item',true)
      .classed('active',function(d,i){return i-1 ==0;})
      .on('click', onRunConfigClick)
      .html(function(d){return d.name;});
      
  runconfigListItems.html(function(d){return d.name;});
      
  runconfigListItems.exit().remove();

}


$(document).ready(function () {

  $('a[data-toggle="tab"][href="#networkModalTabContent"]').on('shown.bs.tab', function (e) {
    $('#SettingsNextButton').addClass('hidden');
    $('#SettingsSaveButton').removeClass('hidden');
  });
  $('#addConfigButton').on('click',addRunConfig);
  $('#SettingsSaveButton').on('click', saveConfig);
  $('#editSettingsButton').on('click', editConfig);
  $('#removeConfigButton').on('click', removeConfig);
});