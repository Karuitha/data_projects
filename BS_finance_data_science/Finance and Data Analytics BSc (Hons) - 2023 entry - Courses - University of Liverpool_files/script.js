$(document).ready(function(){
	var path = document.URL; 

	// ----- hide any post content from wordpress that has broken caption elements ----- //
	if ($(".wp-post").length > 0){
		var imgGrab;
		imgGrab = $('.wp-post p:contains([caption) a').html();
		$('.wp-post p:contains([caption)').remove();
		$('.wp-post p:contains([/caption)').remove();
		$(imgGrab).insertAfter('.meta');
	}

	// ---- load news stories onto the home page of sites ----- //

if (document.getElementById('url-loader')){	
	var firstStory;
	$('#url-loader').load('news/index.htm .content .from-wp:first a', function(){
		firstStory = $('#url-loader a').attr('href');

		//nasty hacky string manipulation to deal with ie7 and it's absolute path issues
		firstStory = (firstStory).replace(/(.*)uk\//,'');
		firstStory = (firstStory).replace(/(.*)articles/,'articles');

		$('#news-load').hide().load('news/' + firstStory + 'index.htm .ie h2:first, .ie img:first, .ie p:eq(3)', function(){
			$('#home-page-news h1').insertAfter('#news-load img');
			$('#news-load h2:first').insertAfter('#news-load h1');
			$('#news-load p').append('<br/> <a class="read-more" href="news/' + firstStory + 'index.htm">Read the full story</a>').insertAfter('#news-load h2:first');
			$('#news-load').fadeIn(function(){
				//make all the expander boxes the same height - just cosmetic
				var arr = new Array(2);
				arr[0] = $('.three-column-module:eq(0) .expander').height();
				arr[1] = $('.three-column-module:eq(1) .expander').height();
				arr[2] = $('.three-column-module:eq(2) .expander').height();
				var largest = arr.sort(function(a,b){return a - b}).slice(-1);
				$('.expander').height(largest);
			});
		});
	});
}

	//----------hide any caption elements on departmental homepage news -----------//
	$('#news-load p:contains([caption)').remove();

	//----------hide caption elements on any departmental homepage article ---------//
	$('.ie p:contains([callout)').remove();
	$('.ie p:contains([/callout)').remove();
	$('.ie div:contains([callout)').remove();

	// ----- re-order the list of news items based on their date stamp
	if (document.getElementById('news-list-sort')){
		var items = $('#news-list-sort li').get();
		items.sort(function(a, b) {
			var valueA = $(a).find("span").text();
			var valueB = $(b).find("span").text();
			if (valueA < valueB)
				{
					return 1;
				}
			else if (valueA > valueB)
				{
					return -1;
				}
			else
				{
					return 0;
				}
			});
		$("#news-list-sort").empty().append(items);
	}

	// ----- re-order the list of news items based on their date stamp (2016)

	if (document.getElementById('news-list-sort2')){
		var items = $('#news-list-sort2 div').get();
		items.sort(function(a, b) {
			var valueA = $(a).find("p.date").text();
			var valueB = $(b).find("p.date").text();
			if (valueA < valueB)
				{
					return 1;
				}
			else if (valueA > valueB)
				{
					return -1;
				}
			else
				{
					return 0;
				}
			});
		$("#news-list-sort2").empty().append(items);
	}

	// ----- re-format date on news list ----- //

	if (document.getElementById('news-list-sort')){
		var m_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
		$('#news-list-sort li span').css('display', 'none').each(function(){
			var postDate = $(this).text();
			var temp = postDate.split(' ');
			temp = temp[0].split ('-');
			var singMonth = temp[1];
			singMonth = singMonth -1;
			$(this).text(temp[2] + " " + m_names[singMonth] + " " +  temp[0]).css('display','block');
		});

		$('.content time').each(function(){
			var postDate = $(this).text();
			var temp = postDate.split(' ');
			temp = temp[0].split ('-');
			var singMonth = temp[1];
			singMonth = singMonth -1;
			$(this).text(temp[2] + " " + m_names[singMonth] + " " +  temp[0]);
		});
	}

	// ----- re-format date on news list (2016) ----- //

	if (document.getElementById('news-list-sort2')){
		var m_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
		$('#news-list-sort2 div p.date').css('display', 'none').each(function(){
			var postDate = $(this).text();
			var temp = postDate.split(' ');
			temp = temp[0].split ('-');
			var singMonth = temp[1];
			singMonth = singMonth -1;
			$(this).text(temp[2] + " " + m_names[singMonth] + " " +  temp[0]).css('display','block');
		});

		$('.content time').each(function(){
			var postDate = $(this).text();
			var temp = postDate.split(' ');
			temp = temp[0].split ('-');
			var singMonth = temp[1];
			singMonth = singMonth -1;
			$(this).text(temp[2] + " " + m_names[singMonth] + " " +  temp[0]);
		});
	}

	// ----- update the paths in imported jobs lists to be absolute ----- //

	if (document.getElementById("job-import-research"))
		{
			$('table[summary=Vacancies] a').each(function(){
				var urlGrab = $(this).attr('href');
				$(this).attr('href','//www.liverpool.ac.uk/working/job_vacancies/research/' + urlGrab);
			});
		}

	// ----- show/hide animation for CPD cours list ----- //
	if (document.getElementById("cpd-course-list"))
		{
			$('<span class="see-all"><a href="#">More information</a></span>').insertAfter('.cpd-overview');
			
			//if someone clicks on a more link reveal the course details
			$('.cpd-overview + span a').live('click', function(event){
				event.preventDefault();
				$(this).parent().prev('.cpd-overview').children('.further-info-wrapper').slideDown('slow');
				$(this).text('Hide information');
				$(this).parent().addClass('hide-pubs').removeClass('see-all');
			});

			// and hide if they click again
			$('.cpd-overview + span.hide-pubs a').live('click', function(event){
				event.preventDefault();
				$(this).parent().prev('.cpd-overview').children('.further-info-wrapper').slideUp('slow');
				$(this).text('More information');
				$(this).parent().addClass('see-all').removeClass('hide-pubs');
			});

			// re-instate the zebra striping that's being knocked out by the javascript interfering with nth child css rule
			$('.cpd-overview:odd').addClass('odd');
		}

	if (document.getElementById("ucas-list"))
		{
			$('<span class="see-all"><a href="#">Essential Requirements</a></span>').insertAfter('.cpd-overview');

			//if someone clicks on a more link reveal the course details
			$('.cpd-overview + span a').live('click', function(event){
				event.preventDefault();
				$(this).parent().prev('.cpd-overview').children('.further-info-wrapper').slideDown('slow');
				$(this).text('Hide Requirements');
				$(this).parent().addClass('hide-pubs').removeClass('see-all');
			});

			// and hide if they click again
			$('.cpd-overview + span.hide-pubs a').live('click', function(event){
				event.preventDefault();
				$(this).parent().prev('.cpd-overview').children('.further-info-wrapper').slideUp('slow');
				$(this).text('Essential Requirements');
				$(this).parent().addClass('see-all').removeClass('hide-pubs');
			});

			// re-instate the zebra striping that's being knocked out by the javascript interfering with nth child css rule
			$('.cpd-overview:odd').addClass('odd');
		}


	// ----- animation for modules on course pates ----- //
	// choose text for the show/hide link - can contain HTML (e.g. an image)

	var showText='<b>[&nbsp;+&nbsp;more&nbsp;]</b>';
	var hideText='<b>[&nbsp;-&nbsp;less&nbsp;]</b>';

	// initialise the visibility check
	var is_visible = false;

	// append show/hide links to the element directly preceding the element with a class of "toggle"
	$('.toggle').prev().append(' <a href="#" class="toggleLink">'+showText+'</a>');

	// hide all of the elements with a class of 'toggle'
	$('.toggle').hide();

	// capture clicks on the toggle links
	$('a.toggleLink').click(function() {

	// switch visibility
	is_visible = !is_visible;

	// change the link depending on whether the element is shown or hidden
	$(this).html( (!is_visible) ? showText : hideText);

	// toggle the display - uncomment the next line for a basic "accordion" style
	//$('.toggle').hide();$('a.toggleLink').html(showText);
	$(this).parent().next('.toggle').toggle('slow');

	// return false so any link destination is not followed
	return false;
	});

	if (document.getElementById("about-fp-wrapper")){
		$('#about-fp-wrapper p:eq(2)').text("As one of the UK's top 24 research-led universities, we are furthering knowledge with strategic partners worldwide.");
	}

});

$('#news-load').remove();

// This function passes query from search page to courses page
$(document).ready(function(){
	function setSearchValue(){
		if(sessionStorage.getItem("storedSearch") !==null){
			$('#master-search input[type=text]').val(sessionStorage.getItem("storedSearch"));
		}
	}

	function watchSearchTerm(){
		$('#master-search input[type=text]').keyup(function(){
			var searchString = $('#master-search input[type=text]').val();
			sessionStorage.setItem("storedSearch", searchString);
		});
	}

	if (document.getElementById('master-search')){
		setSearchValue();
		watchSearchTerm();
	}
});

// Share buttons
	$("a.share").click(function(e)
		{
			e.preventDefault();

			var $lightbox = $("<div id='sharecover'></div>"),
				$sharebox = $("<div id='sharebox'><div></div></div>"),
				$internal = $("<div class='links'></div>"),
				$this = $(this);

			// Content
			$("div", $sharebox).append("<h2>Share via:</h2>");
			$("div", $sharebox).append("<a class='ir close'>x</a>");
			$internal.append("<a class='ir facebook' href='https://www.facebook.com/sharer/sharer.php?u="+encodeURIComponent($this.attr("href"))+"'>Facebook</a>");
			$internal.append("<a class='ir twitter' href='https://twitter.com/intent/tweet?url="+$this.attr("href")+"&text="+$this.data("text")+"&via=livuni'>Twitter</a>");
			$internal.append("<a class='ir googleplus' href='https://plus.google.com/share?url="+$this.attr("href")+"&text="+$this.data("text")+"&via=livuni'>Google+</a>");
			$internal.append("<a class='ir email' href='mailto:?subject="+$this.data("text")+"&body="+$this.attr("href")+"'>Email</a>");

			// Appendings
			$("div", $sharebox).append($internal);
			$("body").append($lightbox);
			$("body").append($sharebox);

			// Close lightbox
			$lightbox.click(function(e)
			{
				e.preventDefault();
				$lightbox.remove();
				$sharebox.remove();
			});

			// Close lightbox via click
			$("div a.close", $sharebox).click(function(e)
			{
				e.preventDefault();
				$lightbox.remove();
				$sharebox.remove();
			});

			// Twitter/facebook button
			$(".tweet, .facebook, .googleplus", $internal).click(function(e)
			{
				e.preventDefault();
				window.open($(this).attr("href"), "Share", "height=250,width=600");
			});
		});

$(document).ready(function () {	
	$('#toggle-view li').click(function () {
		var text = $(this).children('div.panel');
		if (text.is(':hidden')) {
			text.delay(50).slideDown('100');
			$(this).children('span').html('<img src="//www.liverpool.ac.uk/files/images/staff-profiles/toggle-minus-icon.png" alt="close-content-icon" />');		
		} else {
			text.delay(50).slideUp('100');
			$(this).children('span').html('<img src="//www.liverpool.ac.uk/files/images/staff-profiles/toggle-icon.png" alt="open-content-icon" />');		
		}
	});
});