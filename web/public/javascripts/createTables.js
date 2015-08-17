$(document).ready(function () {
  createNetworkTable();
  createClientTable();
  createProtocolSpecificTable();
});

var networkTable;
var clientTable;
var protocolSpecificTable;

function createNetworkTable(){
      networkTable = $('#networkTable').DataTable({
      columns:  [
         {data: 'name'},
         {data: 'packets dispatched'},
         {data: 'packets received'},
         {data: 'packets dropped due to node failure'},
         {data: 'packets dropped due to partition or hub failure'},
         {data: 'packets inflight'},
         {data: 'number of servers'},
         {data: 'number of clients'},
         {data: 'number of startups'},
         {data: 'number of failures'},
         {data: 'number of recoveries'},
         {data: 'average latency'},
         {data: 'min latency'},
         {data: 'max latency'},
      ],
      render: function( data, type, full, meta){
              if (data == null){
                return '';
              }
              else{
                return JSON.stringify(data);
              }
            },
      responsive: false,
      "scrollX": true,
      paginate: false,
    });
}

function addRowToNetworkTable(rowData){
  try{
    networkTable.row.add(rowData);
    networkTable.draw();
  }
  catch(exception){
    console.log('error adding row to network table:');
    console.log(exception);
  }
}

function createClientTable(){
      clientTable = $('#clientTable').DataTable({
      columns:  [
         {data: 'name'},
         {data: 'commands attempted'},
         {data: 'successful'},
         {data: 'failed'},
         {data: 'outstanding'},
         {data: 'average commands applied per state machine'},
         {data: 'average time to successful commit'},
         {data: 'min time to successful commit'},
         {data: 'max time to successful commit'},
      ],
      render: function( data, type, full, meta){
              if (data == null){
                return '';
              }
              else{
                return JSON.stringify(data);
              }
            },
      responsive: false,
      "scrollX": true,
      paginate: false,
    });
}

function addRowToClientTable(rowData){
  try{
    clientTable.row.add(rowData);
    clientTable.draw();
  }
  catch(exception){
    console.log('error adding row to client table:');
    console.log(exception);
  }
}

function createProtocolSpecificTable(){
      protocolSpecificTable = $('#protocolSpecificTable').DataTable({
      columns:  [
         {data: 'name'},
         {data: 'append entries received'},
         {data: 'append entries dispatched'},
         {data: 'append entries requests received'},
         {data: 'append entries requests dispatched'},
         {data: 'append entries responses received'},
         {data: 'append entries responses dispatched'},
         {data: 'request votes received'},
         {data: 'request votes dispatched'},
         {data: 'request votes requests received'},
         {data: 'request votes requests dispatched'},
         {data: 'request votes responses received'},
         {data: 'request votes responses dispatched'},
         {data: 'client request received'},
         {data: 'client request dispatched'},
         {data: 'client request requests received'},
         {data: 'client request requests dispatched'},
         {data: 'client request responses received'},
         {data: 'client request responses dispatched'},
         {data: 'time to first leader'},
         {data: 'elections started'},
         {data: 'elections won'},
         {data: 'elections lost due to insuffient votes'},
         {data: 'elections lost due to step down'},
         {data: 'elections lost due to candidate failure'},
         {data: 'highest term'},
         {data: 'number of commands received'},
         {data: 'number of commands dispatched in AppendEntries'},
         {data: 'number of node failures'},
         {data: 'number of leader failures'},
      ],
      render: function( data, type, full, meta){
              if (data == null){
                return '';
              }
              else{
                return JSON.stringify(data);
              }
            },
      responsive: false,
      "scrollX": true,
      paginate: false,
    });
}

function addRowToProtocolSpecificTable(rowData){
  try{
    protocolSpecificTable.row.add(rowData);
    protocolSpecificTable.draw();
  }
  catch(exception){
    console.log('error adding row to protocol specific table:');
    console.log(exception);
  }
}