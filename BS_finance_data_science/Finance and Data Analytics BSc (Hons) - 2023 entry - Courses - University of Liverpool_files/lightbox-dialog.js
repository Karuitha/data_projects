var current_stream_url;
var current_stream_position;
var current_stream_duration;
var current_stream_percentage;
var current_stream_interval;

jQuery(function() {
	
	window['GoogleAnalyticsObject'] = 'ga';
	window['ga'] = window['ga'] || function() {
		(window['ga'].q = window['ga'].q || []).push(arguments)
	};
	ga('create', 'UA-2638755-3', 'auto');
	
	function lightbox_close() {
		lightbox_type = jQuery('#lightbox-overlay').data('lightbox-type');


		// Trigger events
		if (lightbox_type == 'stream') {
			p = jwplayer('jwplayer').getPosition();
			d = jwplayer('jwplayer').getDuration();
			url = jwplayer('jwplayer').getPlaylistItem().aboutlink ? jwplayer('jwplayer').getPlaylistItem().aboutlink : jwplayer('jwplayer').getConfig().aboutlink;
			ga('send', 'event', 'Stream Video', 'End', url, Math.round(p/d*100));
			if (current_stream_url != '') {
				ga('send', 'event', 'Stream Video', 'End', current_stream_url, current_stream_percentage);
				console.log(current_stream_url, current_stream_percentage);
			}
		}


		// Stop and destroy videos
		if (lightbox_type == 'stream') {
			jwplayer('jwplayer').remove();
			jQuery('#jwplayer').remove();
			clearInterval(current_stream_interval);
			current_stream_url = '';
			jQuery('#lightbox-content').html('');
		} else if (lightbox_type == 'vudoo') {
			jQuery('#lightbox-content .vudoo--embed').remove();
		} else if (lightbox_type == 'category-page') {
			jQuery('#lightbox-content').html('');
		}
		
		
		// Hide lightbox
		document.getElementById('lightbox-overlay').close();
	}
	



	jQuery('<dialog id="lightbox-overlay" ><button id="lightbox-close" class="close">Close</button><h3 id="lightbox-heading"></h3><article id="lightbox-content"></article></dialog>').appendTo('body');

	jQuery('#lightbox-overlay').on('close', lightbox_close);
	// jQuery('#lightbox-close').on('click', lightbox_close);
	
//	jQuery('#lightbox-overlay').on('click', function(e){ if (e.target.id=='lightbox-overlay') lightbox_close();});




	// Category page links

	jQuery('.category-page .category-items a[href*="'+window.location.pathname+'"]').on('click', function(e){
		e.stopPropagation();
		page_url = new URL(this.href);
		jQuery('#lightbox-content').load(page_url+' #main-content');
//		jQuery.get(page_url+'#main-content', function(data){jQuery('#lightbox-content').html(data);});
		jQuery('#lightbox-overlay').data('lightbox-type', 'category-page');
		document.getElementById('lightbox-overlay').showModal();
		jQuery('#lightbox-close').focus();
		
		// Fire Google Analytics event
		ga('send', 'pageview', page_url.pathname);
		
		return false;
	});
	

	// Profile

	jQuery('a[data-profile-url]').on('click', function(e){
		e.stopPropagation();
		page_url = jQuery(this).data('profile-url');
		console.log(page_url);
		jQuery('#lightbox-heading').html(this.dataset.dialogHeading);
		jQuery('#lightbox-content').load(page_url, function(){console.log(jQuery(this).find('h2').first());});
//		jQuery.get(page_url+'#main-content', function(data){jQuery('#lightbox-content').html(data);});
//jQuery('#lightbox-heading').replaceWith(jQuery('#lightbox-content').find('h2').first().attr('id', 'lightbox-heading'));
		jQuery('#lightbox-overlay').data('lightbox-type', 'category-page');
		document.getElementById('lightbox-overlay').showModal();
		jQuery('#lightbox-close').focus();
		
		// Fire Google Analytics event
		ga('send', 'pageview', page_url.pathname);
		
		return false;
	});
	



	// Vudoo links

	jQuery('a[href^="https://university-of-liverpool.vudoo.io/watch/"]').on('click', function(e){
		e.stopPropagation();
		stream_url = new URL(this.href);
		video_id = stream_url.pathname.replace('/watch/', '').replace('/embed/', '');
		console.log(video_id);
		jQuery('<div class="vudoo--embed"><iframe id="vudoo-iframe" width="640" height="360" src="https://university-of-liverpool.vudoo.io/embed/'+video_id+'?autoplay=1" frameborder="0"  allow="autoplay" allowfullscreen ></iframe></div>').appendTo('#lightbox-content');
		document.getElementById("vudoo-iframe").onload = function() {var vudooFrame = new vudooIframeEmbed({frame:this,embedid:video_id});};var toggle_frame_screen=function(e,t){var l=document.getElementById("vudoo-iframe");e?(l.style.position="fixed",l.style.bottom="0",l.style.left="0",l.style.width="100vw",l.style.height="100%",l.style.zIndex="500000"):(l.style.position="",l.style.top="",l.style.bottom="",l.style.left="",l.style.width="",l.style.height="",l.style.maxWidth="100%",l.style.zIndex="");};
		jQuery('#lightbox-overlay').data('lightbox-type', 'vudoo');
		document.getElementById('lightbox-overlay').showModal();
		jQuery('#lightbox-close').focus();
		
		// Fire Google Analytics event
		ga('send', 'event', 'Vudoo Video', 'Play', this.href);
		
		return false;
	});
	


	// Stream links

	jQuery('a[href^="https://stream.liv.ac.uk/"]').on('click', function(e){
		e.stopPropagation();
		var a_tag = this;
		jQuery('#lightbox-heading').html(a_tag.title);
		jQuery('#lightbox-heading').html(this.dataset.dialogHeading);
		stream_url = new URL(this.href);
		video_id = stream_url.pathname.replace('/s/', '/').replace('/', '');
		console.log(video_id);
		jQuery('<div id="jwplayer" />').appendTo('#lightbox-content');
		jQuery('#lightbox-overlay').data('lightbox-type', 'stream');
		document.getElementById('lightbox-overlay').showModal();
		jQuery('#lightbox-close').focus();

		if ( jQuery(this).parents('.playlist').length ) { // Part of a playlist

			console.log('Part of a playlist');
			//console.log(jQuery(this).parents('.playlist').children('a'));


			var playlist = [];
			var start_position = 0;
			jQuery(this).parents('.playlist').find('a[href^="https://stream.liv.ac.uk/"]').each(function(i){
				i_tag = this;
				stream_url = new URL(this.href);
				v_id = stream_url.pathname.replace('/s/', '/').replace('/', '');

				tracks = [];
				if (jQuery(this).data('captions')) {
					tracks.push({file: jQuery(this).data('captions'), label: 'English', kind: 'captions'});
				} else {
					tracks.push({ file:'https://stream.liv.ac.uk/captions/'+v_id+'_english.vtt', label: 'English', kind: 'captions' });
				}
				if (stream_url.searchParams.get('chapters')) {
					tracks.push({file: stream_url.searchParams.get('chapters'), label: 'Chapters', kind: 'chapters'});
				}
				playlist.push({
					sources: [{
						file: 'https://588def42838ee.streamlock.net:443/vod/_definst_/smil:'+v_id+'/'+v_id+'.smil/manifest.mpd' },{
						file: 'https://588def42838ee.streamlock.net:443/vod/_definst_/smil:'+v_id+'/'+v_id+'.smil/playlist.m3u8' }
					],
					tracks: tracks,
					title: this.text,
					description: this.title,
					image: 'https://stream.liv.ac.uk/thumbnails/'+v_id+'_001.jpg',
					starttime: stream_url.searchParams.get('s'),
					aboutlink: this.href
				});
				if (a_tag == i_tag) {
					start_position = i;
				}
				console.log(a_tag, i_tag, start_position);
			});
				jwplayer('jwplayer').setup({
				//			title: 'VideoTitle',
					//		description: 'VideoDescription',
							//'height': '405',
							playlist: playlist,
							playlistIndex: start_position,
							ga: {},
							width: "100%",
							aspectratio: "16:9",
							stretching: "uniform",
							skin: {name: 'roundster'},
							abouttext: 'Watch video on stream.liv.ac.uk',
							aboutlink: this.href,
							//'image': '',
							hlslabels: {'500': 'SD (360p)','2000': 'HD (720p)'},
							proxyType: 'best',
							autostart: 'true',
							//playbackRateControls: [2, 1.75, 1.5, 1.25, 1, 0.75, 0.5],
							
						});
				
				// Fire additional GA events when playlist changes
				jwplayer('jwplayer').on('playlistItem', function(o){
					if (current_stream_url) {
						ga('send', 'event', 'Stream Video', 'End', current_stream_url, current_stream_percentage);
						console.log(current_stream_url, current_stream_percentage);
					}
					current_stream_url = o.item.aboutlink;
					current_stream_position = 0;
					current_stream_percentage = 0;
					ga('send', 'event', 'Stream Video', 'Play', o.item.aboutlink);

				});

				current_stream_interval = setInterval(function(){
					current_stream_position = jwplayer('jwplayer').getPosition();
					current_stream_duration = jwplayer('jwplayer').getDuration();
					current_stream_percentage = Math.round(current_stream_position/current_stream_duration*100);
				}, 1000);

		} else { // Single video
			tracks = [];
			if (jQuery(this).data('captions')) {
				tracks.push({file: jQuery(this).data('captions'), label: 'English', kind: 'captions'});
			} else {
				tracks.push({ file:'https://stream.liv.ac.uk/captions/'+v_id+'_english.vtt', label: 'English', kind: 'captions' });
			}
			if (stream_url.searchParams.get('chapters')) {
				tracks.push({file: stream_url.searchParams.get('chapters'), label: 'Chapters', kind: 'chapters'});
			}

			jwplayer('jwplayer').setup({
				title: a_tag.title,
		//		description: 'VideoDescription',
				//'height': '405',
				ga: {},
				width: "100%",
				aspectratio: "16:9",
				stretching: "uniform",
				skin: {name: 'roundster'},
				abouttext: 'Watch video on stream.liv.ac.uk',
				aboutlink: this.href,
				//'image': '',
				sources: [{
					file: 'https://588def42838ee.streamlock.net:443/vod/_definst_/smil:'+video_id+'/'+video_id+'.smil/manifest.mpd' },{
					file: 'https://588def42838ee.streamlock.net:443/vod/_definst_/smil:'+video_id+'/'+video_id+'.smil/playlist.m3u8' }
				],
				hlslabels: {'500': 'SD (360p)','2000': 'HD (720p)'},
				proxyType: 'best',
				autostart: 'true',
				//playbackRateControls: [2, 1.75, 1.5, 1.25, 1, 0.75, 0.5],
				tracks: tracks,
				starttime: stream_url.searchParams.get('s')
			});
			current_stream_url = this.href;
			current_stream_interval = setInterval(function(){
				current_stream_position = jwplayer('jwplayer').getPosition();
				current_stream_duration = jwplayer('jwplayer').getDuration();
				current_stream_percentage = Math.round(current_stream_position/current_stream_duration*100);
			}, 1000);
		}


		// Fire Google Analytics event
		ga('send', 'event', 'Stream Video', 'Play', this.href);

		// Fire additional GA events when video completes
		jwplayer('jwplayer').on('complete', function(e) {
			ga('send', 'event', 'Stream Video', 'Complete', jwplayer('jwplayer').getConfig().playlistItem.aboutlink);
		});
	  
		return false;
	});

	// Add data to stream links

	jQuery('a[href^="https://stream.liv.ac.uk/"]').each(function(i){
		var link = this;
		stream_url = new URL(this.href);
		console.log();
		v_id = stream_url.pathname.replace('/s/', '/').replace('/', '');
		var captions = 'https://stream.liv.ac.uk/captions/'+v_id+'_english.srt';
		$.ajax({
			url: captions,
			type:'HEAD',
			error: function()
			{
				//file not exists
			},
			success: function()
			{
				jQuery(link).attr('data-captions', captions);

			}
		});
	});


});