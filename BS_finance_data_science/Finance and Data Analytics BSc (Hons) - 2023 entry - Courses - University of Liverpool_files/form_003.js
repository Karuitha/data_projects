;(function() {

    // Object.assign Pollyfill
    if (typeof Object.assign !== 'function') {Object.defineProperty(Object, "assign", {value: function assign(target, varArgs) {'use strict'; if (target === null || target === undefined) {throw new TypeError('Cannot convert undefined or null to object'); } var to = Object(target); for (var index = 1; index < arguments.length; index++) {var nextSource = arguments[index]; if (nextSource !== null && nextSource !== undefined) {for (var nextKey in nextSource) {if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {to[nextKey] = nextSource[nextKey]; } } } } return to; }, writable: true, configurable: true }); }

    var initFn = function(scriptRef) {
        // Insert iframe
        try {
            var serialize = function(url) {
                var vars = {};
                var parts = (url || '').replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m, key, value) {
                    vars[key] = value;
                });
                return vars;
            }

            // Get window query param
            var getWindowQueryParam = function(param) {
                return serialize(window.location.href)[param] || '';
            }

            // Get cached param
            var getCachedParam = function(key) {
                try {
                    if (window.localStorage) {
                        return localStorage.getItem(key);
                    } else {
                        var cookieData = {};
                        var cookies = document.cookie.split(';');
                        for (var i = 0; i < cookies.length; i++) {
                            var parts = cookies[i].split('=');
                            cookieData[parts[0].trim()] = parts[1].trim();
                        }
                        return cookieData[key];
                    }
                } catch (err) { 
                    console.log(err);
                    return null;
                }
            }

            var setCachedParam = function(key, value) {
                try {
                    if (window.localStorage) {
                        localStorage.setItem(key, value);
                    } else {
                        document.cookie = key + '=' + value + ';max-age=86400;path=/';
                    }
                } catch (err) { console.log(err); }
            }

            var getParam = function(key) {
                // Return query params first
                if (getWindowQueryParam(key)) {
                    return getWindowQueryParam(key);
                } else if (getCachedParam(key)) {
                    return getCachedParam(key);
                }
                return;
            }

            // IE10 Polyfill
            if (window.location.origin == undefined) {
                window.location.origin = window.location.protocol + "//" + window.location.hostname + (window.location.port ? ':' + window.location.port: '');
            }

            var scriptElement = scriptRef;

            // Build iframe src
            var baseUrl = scriptElement.src.replace(/\/gecko-embed\/.+$/, '');
            var uuid    = scriptElement.src.match(/uuid=([^&]*)/)[1];
            
            // Dont load if no baseUrl, uuid or page origin is found
            if (!baseUrl || !uuid || !window.location.origin) return;

            // Check for pre-populate in query string on both script src and window
            var scriptParams = serialize(scriptElement.src);
            var windowParams = serialize(window.location.href);
            var mergedParams = Object.assign({}, scriptParams, windowParams);
            // Window params should overwrite params set on the embed script
            var prepopulateParams = Object.keys(mergedParams).filter(function(key) {
                return [
                    'uuid',
                    'utm_source',
                    'utm_medium',
                    'utm_campaign',
                    'utm_term',
                    'utm_content'
                ].indexOf(key) === -1;
            }).map(function(key) {
                return key + '=' + mergedParams[key];
            });

            // Get UTM params
            var utm_source   = getParam('utm_source');
            var utm_medium   = getParam('utm_medium');
            var utm_campaign = getParam('utm_campaign');
            var utm_term     = getParam('utm_term');
            var utm_content  = getParam('utm_content');

            // Cache UTM params
            if (utm_source)   setCachedParam('utm_source', utm_source);
            if (utm_medium)   setCachedParam('utm_medium', utm_medium);
            if (utm_campaign) setCachedParam('utm_campaign', utm_campaign);
            if (utm_term)     setCachedParam('utm_term', utm_term);
            if (utm_content)  setCachedParam('utm_content', utm_content);

            // Build UTM params (from window url or cache)
            var utmParams = [];
            if (utm_source)   utmParams.push('utm_source='+utm_source);
            if (utm_medium)   utmParams.push('utm_medium='+utm_medium);
            if (utm_campaign) utmParams.push('utm_campaign='+utm_campaign);
            if (utm_term)     utmParams.push('utm_term='+utm_term);
            if (utm_content)  utmParams.push('utm_content='+utm_content);

            // Pass page origin to iframe (vua query_string)
            queryString = '?';
            queryString += 'origin=' + encodeURI(window.location.origin);
            // Add pre-populate params
            if (prepopulateParams.length) queryString += ('&' + prepopulateParams.join('&'));
            // Add (window) utm params
            if (utmParams.length) queryString += ('&' + utmParams.join('&'));

            // Build iframe element
            var iframe = document.createElement('iframe');

            iframe.src         = baseUrl + '/public/#/iframe/' + uuid + queryString;
            iframe.id          = 'gecko-form-iframe-embed-' + uuid;
            iframe.width       = '100%';
            // Default height required for IE11/IE10
            iframe.height      = '500';
            //Dont allow iframe scrolling (unless overwritten)
            if (!(scriptElement.src.match(/scrolling=([^&]*)/) && scriptElement.src.match(/scrolling=([^&]*)/)[1])) {
                iframe.setAttribute('scrolling', 'no');
            }
            iframe.setAttribute('frameborder', '0');
            iframe.setAttribute('seamless', '');
            iframe.setAttribute('title', 'GeckoForm Form Embed');

            // Add iframe to page
            scriptElement.parentElement.insertBefore(iframe, scriptElement.nextSibling);
        } catch (e) { console.log(e); }
    }

    var bootFn = function() {
        var scripts = document.querySelectorAll('[id=gecko-form-embed-script]');
        for (var i=0; i<scripts.length; i++) {
            var scriptRef = scripts[i];
            // Check if script has already fired
            if (!scriptRef.getAttribute('data-gecko-embed-inited')) {
                // Mark script as inited
                scriptRef.setAttribute('data-gecko-embed-inited', '1');
                initFn(scriptRef);
            }
        }
    };

    bootFn();

}());

;(function(){
	try {
		// Get iframe element
		function iframe(uuid) {
			var id = 'gecko-form-iframe-embed-' + uuid;
            return document.querySelectorAll('[id='+id+']');
		}

        // Run a function for each iframe on the page (that matches a uuid)
        function forEachIframe(uuid, cb) {
            var iframes = iframe(uuid);
            for (var i=0; i<iframes.length; i++) {
                cb(iframes[i]);
            }
        }

		// Set iFrame height
		function setGeckoIframeHeight(event){
			if (event.data && event.data.formIframe && event.data.uuid && event.data.height) {
                forEachIframe(event.data.uuid, function(iframeRef) {
                    iframeRef.height       = event.data.height + 'px';
                    iframeRef.style.height = event.data.height + 'px';
                    // Used for embeded events :(
                    if (window.eventsEmbedFormHeightUpdate) window.eventsEmbedFormHeightUpdate();
                });
			}
		}
		// Scroll window to top of iframe (or offset if set)
		function setGeckoIframePosition(event){
			if (event.data && event.data.formIframe && event.data.uuid && event.data.scroll) {
                forEachIframe(event.data.uuid, function(iframeRef) {
                    if (window.pageYOffset > iframeRef.offsetTop || event.data.offset) {
                        window.scrollTo(0, iframeRef.getBoundingClientRect().top + window.pageYOffset + event.data.offset);               
                    }
                });
			}
		}
		// Pass the window scroll position 
		function getGeckoIframeScrollPos(event) {
			if (event.data.uuid && event.data.getScrollPos) {
                forEachIframe(event.data.uuid, function(iframeRef) {
                    iframeRef.contentWindow.postMessage({
                        iframeTop: iframeRef.getBoundingClientRect().top + window.pageYOffset,
                        scrollTop: window.pageYOffset,
                    }, event.origin);
                });
			}
		}

		// Pass GA clientId
		function getGaClientId(event) {
			try {
				if (event.data.uuid && event.data.getGaClientId) {
                    forEachIframe(event.data.uuid, function(iframeRef) {
                        var clientId = window.ga && window.ga.getAll().length ? window.ga.getAll()[0].get('clientId') : null;
                        iframeRef.contentWindow.postMessage({
                            gaClientId: clientId
                        }, event.origin);
                    });
				}
			} catch (err) {
				console.log(err);
			}			
		}

        // Event listeners
        var listeners = [
            setGeckoIframeHeight,
            setGeckoIframePosition,
            getGeckoIframeScrollPos,
            getGaClientId
        ];

        for (var i=0; i<listeners.length; i++) {
            if (window.addEventListener) {
                window.addEventListener('message', listeners[i]);
            } else {
                window.attachEvent('onmessage', listeners[i]);
            }
        }

	} catch (e) { console.log(e); }
}());