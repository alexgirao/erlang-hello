Nabble._uacct = "UA-91855-8";

Nabble.analytics = function() {
	if (navigator.cookieEnabled && !Nabble.getCookie("v")) {
		var visitCounter = "/util/VisitCounter.jtp?referrer=" + encodeURIComponent(document.referrer);
		var a = /:\/\/(.*?)\//.exec(document.referrer);
		var hostname = a ? a[1] : 'none';
		Nabble.loadScript(visitCounter);
		Nabble.urchinTracker('/referrer/'+hostname);
	}
	var expires = new Date();
	expires.setTime(expires.getTime()+30*60*1000);
	document.cookie = "v=x; expires=" + expires.toGMTString() + "; path=/";
};

Nabble.urchinTracker = function(page) {
	if (typeof(urchinTracker) != "undefined") {
		_uacct = Nabble._uacct;
		urchinTracker(page);
	}
};

Nabble.ads = function(type,format) {
	if (type == 'NONE')
		return;

	var ad = Math.random();
	document.writeln('<a id="ad' + ad + '" href="#" style="display:none">sample</a>');

	var e = document.getElementById('ad'+ad);
	var narrow = $(e).parent().width() < 728;
	if (type == 'null') Nabble.adsense(format, ad, narrow);
	else if (type == 'ADSENSE') Nabble.adsense(format, ad, narrow);
	else if (type == 'ADBRITE') Nabble.adbrite(format, ad, narrow);
	else if (type == 'GAMBLE') Nabble.gamble(format, ad, narrow);
	else if (type == 'PORN') Nabble.porn(format, ad, narrow);
};

Nabble.adsense = function(format, ad, narrow) {
	var bg = Nabble.getBg(ad);
	google_ad_channel = "";
	google_color_border = bg;
	google_color_bg = bg;
	google_color_text = "808080";
	google_color_url = "808080";
	google_color_link = Nabble.getLinkColor(ad);
	google_ad_client = "ca-pub-7629862567675942";
	
	if (format == "small-square") {
		google_ad_slot = "6393075484";
		google_ad_width = 200;
		google_ad_height = 200;
	} else {
		if (narrow) {
			google_ad_slot = "6696644864";
			google_ad_width = 468;
			google_ad_height = 60;
		} else {
			google_ad_slot = "2373809331";
			google_ad_width = 728;
			google_ad_height = 90;
		}
	}
	document.write('<script type="text/javascript" src="http://pagead2.googlesyndication.com/pagead/show_ads.js"></script>');
}

Nabble.adbrite = function(format, ad, narrow) {
	if (format == "small-square") {
		Nabble.clicksorVars(ad);
		document.write('<script type="text/javascript" src="http://ads.clicksor.com/newServing/showAd.php?nid=1&amp;pid=192225&amp;adtype=8&amp;sid=304446"></script>');
	} else {
		if (narrow) {
			Nabble.clicksorVars(ad);
			document.write('<script type="text/javascript" src="http://ads.clicksor.com/newServing/showAd.php?nid=1&amp;pid=192225&amp;adtype=2&amp;sid=304446"></script>');
		} else {
			var bg = Nabble.getBg(ad);
			var AdBrite_Title_Color = Nabble.getLinkColor(ad);
			var AdBrite_Text_Color = '808080';
			var AdBrite_Background_Color = bg;
			var AdBrite_Border_Color = bg;
			var AdBrite_URL_Color = '808080';
			try{var AdBrite_Iframe=window.top!=window.self?2:1;var AdBrite_Referrer=document.referrer==''?document.location:document.referrer;AdBrite_Referrer=encodeURIComponent(AdBrite_Referrer);}catch(e){var AdBrite_Iframe='';var AdBrite_Referrer='';}
			document.write('<span style="white-space:nowrap;">');
			document.write(String.fromCharCode(60,83,67,82,73,80,84));
			document.write(' src="http://ads.adbrite.com/mb/text_group.php?sid=1982770&zs=3732385f3930&ifr='+AdBrite_Iframe+'&ref='+AdBrite_Referrer+'" type="text/javascript">');
			document.write(String.fromCharCode(60,47,83,67,82,73,80,84,62));
			document.write('<a target="_top" href="http://www.adbrite.com/mb/commerce/purchase_form.php?opid=1982770&afsid=1"><img src="http://files.adbrite.com/mb/images/adbrite-your-ad-here-leaderboard.gif" style="background-color:#CCCCCC;border:none;padding:0;margin:0;" alt="Your Ad Here" width="14" height="90" border="0" /></a></span>');
		}
	}
}

Nabble.gamble = function(format, ad, narrow) {}

Nabble.porn = function(format, ad, narrow) {
	Nabble.clicksorVars(ad);
	if (format == "small-square") {
		document.write('<script type="text/javascript" src="http://ads.clicksor.com/newServing/showAd.php?nid=1&amp;pid=192225&amp;adtype=8&amp;sid=320120"></script>');
	} else {
		document.write('<center>');
		if (narrow) {
			document.write('<script type="text/javascript" src="http://ads.clicksor.com/newServing/showAd.php?nid=1&amp;pid=192225&amp;adtype=2&amp;sid=320120"></script>');
		} else {
			document.write('<script type="text/javascript" src="http://ads.clicksor.com/newServing/showAd.php?nid=1&amp;pid=192225&amp;adtype=1&amp;sid=320120"></script>');
		}
		document.write('</center>');
	}
}

Nabble.clicksorVars = function(ad) {
	var bg = Nabble.getBg(ad);
	clicksor_default_url = '';
	clicksor_banner_border = '#' + bg;
	clicksor_banner_ad_bg = '#' + bg;
	clicksor_banner_link_color = '#808080';
	clicksor_banner_text_color = '#808080';
	clicksor_banner_image_banner = true;
	clicksor_banner_text_banner = true;
	clicksor_layer_border_color = '';
	clicksor_layer_ad_bg = '';
	clicksor_layer_ad_link_color = '';
	clicksor_layer_ad_text_color = '';
	clicksor_text_link_bg = '';
	clicksor_text_link_color = '';
	clicksor_enable_text_link = false;
	clicksor_layer_banner = false;
};

Nabble.getBg = function(ad) {
	var $elem = $(Nabble.get('ad'+ad));
	var bg = $elem.parent().css('background-color');
	if (bg == 'transparent')
		return "FFFFFF";
	if (bg.charAt(0) == '#')
		return bg.substring(1);
	else if (bg.indexOf('rgba') == 0) {
		return "FFFFFF";
	}
	else if (bg.indexOf('rgb') == 0) {
		bg = bg.substring(4, bg.length-1);
		var rgb = bg.split(',');
		return Nabble.hexa(rgb[0]) + Nabble.hexa(rgb[1]) + Nabble.hexa(rgb[2]);
	}
	return 'None: ' + bg;
};

Nabble.getLinkColor = function(ad) {
	var c = $(Nabble.get('ad'+ad)).css('color');
	if (c.charAt(0) == '#')
		return c.substring(1);
	else if (c.indexOf('rgb') == 0) {
		c = c.substring(4, c.length-1);
		var rgb = c.split(',');
		return Nabble.hexa(rgb[0]) + Nabble.hexa(rgb[1]) + Nabble.hexa(rgb[2]);
	}
	return 'EEEEEE';
};

Nabble.hexa = function(c) {
	var n = parseInt(c).toString(16);
	return n.length == 1? '0' + n : n;
};