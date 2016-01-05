function makeRequest(opts) {
  // Return a new promise.
  return new Promise(function(resolve, reject) {
    // Do the usual XHR stuff
    var req = new XMLHttpRequest();
    var url = opts.url;
    if (opts.query) {
       url=url+"?"+opts.query;
    }
    req.open(opts.method, url);

    req.onload = function() {
      // This is called even on 404 etc
      // so check the status
      if (req.status >= 200 && this.status < 300) {
        // Resolve the promise with the response text
        resolve(req.response);
      }
      else {
        // Otherwise reject with the status text
        // which will hopefully be a meaningful error
        reject(Error(req.statusText));
      }
    };

    // Handle network errors
    req.onerror = function() {
      reject(Error("Network Error"));
    };

    if (opts.headers) {
      Object.keys(opts.headers).forEach(function (key) {
        req.setRequestHeader(key, opts.headers[key]);
      });
    }
    var params = opts.params;
    if (params && typeof params == 'object') {
       params = Object.key(params).map(function(key) {
         return encodingURIComponent(key) + "=" +
                  encodeURIComponent(params[key]);
       }).join('&');
    }

    if (opts.timeout) {
        console.log("found timeout: "+opts.timeout);
        req.timeout = opts.timeout;
    }

    // Make the request
    req.send(params);
  });
}

function makeJSONRequest(opts) {
  return makeRequest(opts).then(JSON.parse);
}

function serverGet(key) {
  var opts =
         { method: "GET",
           url: "http://"+location.hostname+"/bastu/"+key,
         };
  return makeJSONRequest(opts);
}

function serverGetPasswd(key, password) {
  var opts =
         { method: "GET",
           url: "http://"+location.hostname+"/bastu/"+key,
           query:  "password="+encodeURIComponent(password),
         };
  return makeJSONRequest(opts);
}

function pubServerGet(key) {
  var opts =
         { method: "GET",
           url: "http://"+location.hostname+"/bastu_pub/"+key,
         };
  return makeJSONRequest(opts);
}

function pubServerLogin(username, password, remember) {
  var opts =
         { method: "GET",
           url: "http://"+location.hostname+"/bastu_pub/login",
           query:  ("username="+username+
                    "&password="+encodeURIComponent(password)+
                    "&remember="+remember)
         };
  return makeJSONRequest(opts);
}
