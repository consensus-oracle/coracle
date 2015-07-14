var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var exec = require('child_process').exec;
var fs = require('fs');
var uuid = require('node-uuid');

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'jade');

// uncomment after placing your favicon in /public
//app.use(favicon(path.join(__dirname, 'public', 'favicon.ico')));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.get('/', function(req, res) {
    res.render('index',{ title: 'Coracle' })
});

app.post('/runSim', function(req,res){
    console.log(req.body);
	var id = uuid.v1();
	var filename = 'requests/' + id + '.json'
	fs.writeFile(filename,JSON.stringify({
		nodes: parseInt(req.body.nodes),
		loss: parseFloat(req.body.loss)	
	}),function(err){
		if(err){
			return console.log(err);
		}
		console.log(filename + ' saved');
	
		var cmd = '../coracle_sim.byte -f ' + filename;
		exec(cmd, function (error,stdout,stderr){
			console.log('stdout: ' + stdout);
			console.log('stderr: ' + stderr);
			res.send(stdout);
		});
	});
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

module.exports = app;
