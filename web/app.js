var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var exec = require('child_process').exec;
var fs = require('fs');
var uuid = require('node-uuid');
var serveIndex = require('serve-index');
var moment = require('moment');

var app = express();

// uncomment after placing your favicon in /public
//app.use(favicon(path.join(__dirname, 'public', 'favicon.ico')));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));
app.use('/requests',serveIndex(path.join(__dirname, 'requests'), {'icons': true}));
app.use('/requests',express.static('requests'));
app.use(express.static(path.join(__dirname, 'views')))


app.get('/examples.json', function(req,res){
  fs.readdir(path.join(__dirname, 'public','examples'), function(err,files){
    console.log(files);
    var templateNames = files.filter(function(d){
      return d.indexOf('.json') != -1;
    })
    .map(function(d){
      return d.split('.')[0];
    });
    res.send({templateNames: templateNames});
  });
});

app.post('/runSim', function(req,res){
    res.header('Access-Control-Allow-Origin', '*');
    console.log('running simulation:');
    console.log(req);
    try{
    	var id = moment().format('YYYY-MM-DD-HH-mm-ss-SSSS');
    	var filename = 'requests/' + id + '.json'
    	fs.writeFile(filename,parseRequest(req),function(err){
    		if(err){
    			return console.log(err);
    		}
    		console.log(filename + ' saved');
    	
    		var cmd = 'OCAMLRUNPARAM=b; ../coracle_sim.byte -f ' + process.cwd() +'/' + filename;
    		console.log('running: ' + cmd);
        var start = new Date();
    		exec(cmd, { timeout: 5000, maxBuffer: 1000*1024} ,function (error,stdout,stderr){
          var end = new Date() - start;
    			console.log('stdout: ' + stdout);
    			console.log('stderr: ' + stderr);
    			res.send({error:error,
    				stdout:stdout,
    				stderr:stderr,
            time:end});
    		});
    	});
    }
    catch(exception){
      console.log(exception);
      res.send({error:exception});
    }
});

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  var err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handlers

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
  app.use(function(err, req, res, next) {
    res.status(err.status || 500);
    res.render('error', {
      message: err.message,
      error: err
    });
  });
}

// production error handler
// no stacktraces leaked to user
app.use(function(err, req, res, next) {
  res.status(err.status || 500);
  res.render('error', {
    message: err.message,
    error: {}
  });
});

function parseRequest(req){
  /*return JSON.stringify({
		nodes: parseInt(req.body.nodes),
		loss: parseFloat(req.body.loss),
		termination: parseInt(req.body.termination),
		seed: parseInt(req.body.seed)
	})
  */
  console.log(JSON.parse(req.body.data));
  return req.body.data;
}

module.exports = app;
