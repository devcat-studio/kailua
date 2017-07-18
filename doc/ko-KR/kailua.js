(function() {

	var languages = [
		{ code: 'en-US', display: 'English', current: ' (selected)' },
		{ code: 'ko-KR', display: '한국어', current: ' (선택됨)' }
	];

	// determine the current language from the URL.
	// if not possible, exit early (should not activate when relocated)
	var currentLanguage = -1;
	var pathNamePrefix, pathNameSuffix;
	for (var i = 0; languages[i]; ++i) {
		var pos = window.location.pathname.indexOf('/' + languages[i].code + '/');
		if (pos >= 0) {
			currentLanguage = i;
			pathNamePrefix = window.location.pathname.substring(0, pos + 1);
			pathNameSuffix = window.location.pathname.substring(pos + languages[i].code.length + 1);
			break;
		}
	}
	if (currentLanguage < 0) return;

	// insert a language menu after the theme button
	$(document).ready(function() {
		$('.left-buttons').append('<i id="language-toggle" class="fa fa-language"></i>');
		$('#language-toggle').click(function() {
			if ($('.language-popup').length) {
				$('.language-popup').remove();
			} else {
				var popup = $('<div class="language-popup" />');
				for (var i = 0; languages[i]; ++i) {
					var item = $('<div class="language" />')
						.attr('lang', languages[i].code)
						.text(languages[i].display + ' ');
					if (i === currentLanguage) {
						item.append($('<span class="default" />').text(languages[i].current));
					}
					popup.append(item);
				}
				popup.insertAfter(this);

				$('.language').click(function(){
					var lang = $(this).attr('lang');
					window.location.pathname = pathNamePrefix + lang + pathNameSuffix;
				});
			}
		});
	});

	// hide theme selector popup when clicking outside of it
	$(document).click(function(event){
		var popup = $('.language-popup');
		if(popup.length) {
			var target = $(event.target);
			if(!target.closest('.language').length && !target.closest('#language-toggle').length) {
				popup.remove();
			}
		}
	});

})();
