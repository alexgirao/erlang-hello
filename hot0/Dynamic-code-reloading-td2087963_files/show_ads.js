
(function(){var g=true,h=null,i=false,j=(new Date).getTime();var k=this,aa=function(a,b){var c=a.split("."),d=k;!(c[0]in d)&&d.execScript&&d.execScript("var "+c[0]);for(var e;c.length&&(e=c.shift());)!c.length&&b!==void 0?d[e]=b:d=d[e]?d[e]:d[e]={}},ba=function(a,b,c){return a.call.apply(a.bind,arguments)},ca=function(a,b,c){if(!a)throw Error();if(arguments.length>2){var d=Array.prototype.slice.call(arguments,2);return function(){var c=Array.prototype.slice.call(arguments);Array.prototype.unshift.apply(c,d);return a.apply(b,c)}}else return function(){return a.apply(b,
arguments)}},l=function(a,b,c){l=Function.prototype.bind&&Function.prototype.bind.toString().indexOf("native code")!=-1?ba:ca;return l.apply(h,arguments)};var da=/&/g,ea=/</g,fa=/>/g,ha=/\"/g,m={"\x00":"\\0","\u0008":"\\b","\u000c":"\\f","\n":"\\n","\r":"\\r","\t":"\\t","\u000b":"\\x0B",'"':'\\"',"\\":"\\\\"},q={"'":"\\'"},ja=function(a){for(var b=0,c=String(ia).replace(/^[\s\xa0]+|[\s\xa0]+$/g,"").split("."),a=String(a).replace(/^[\s\xa0]+|[\s\xa0]+$/g,"").split("."),d=Math.max(c.length,a.length),e=0;b==0&&e<d;e++){var f=c[e]||"",ra=a[e]||"",u=RegExp("(\\d*)(\\D*)","g"),n=RegExp("(\\d*)(\\D*)","g");do{var o=u.exec(f)||["","",""],p=n.exec(ra)||["",
"",""];if(o[0].length==0&&p[0].length==0)break;b=s(o[1].length==0?0:parseInt(o[1],10),p[1].length==0?0:parseInt(p[1],10))||s(o[2].length==0,p[2].length==0)||s(o[2],p[2])}while(b==0)}return b},s=function(a,b){if(a<b)return-1;else if(a>b)return 1;return 0};var ka=document,t=window;var v=function(a){return a=="true"?g:a=="false"?i:i},la=/^([\w-]+\.)*([\w-]{2,})(\:[0-9]+)?$/,w=function(a){return!a?"pagead2.googlesyndication.com":(a=a.match(la))?a[0]:"pagead2.googlesyndication.com"};var z=parseFloat("0"),ma=isNaN(z)||z>1||z<0?0:z;var na=v("true"),oa=v("false"),pa=v("false"),qa=v("false");var sa=function(){return w("")};var A,B,C,E,ta=function(){return k.navigator?k.navigator.userAgent:h};E=C=B=A=i;var F;if(F=ta()){var ua=k.navigator;A=F.indexOf("Opera")==0;B=!A&&F.indexOf("MSIE")!=-1;C=!A&&F.indexOf("WebKit")!=-1;E=!A&&!C&&ua.product=="Gecko"}var G=B,H=E,va=C,I;
a:{var J="",K;if(A&&k.opera)var L=k.opera.version,J=typeof L=="function"?L():L;else if(H?K=/rv\:([^\);]+)(\)|;)/:G?K=/MSIE\s+([^\);]+)(\)|;)/:va&&(K=/WebKit\/(\S+)/),K)var wa=K.exec(ta()),J=wa?wa[1]:"";if(G){var xa,ya=k.document;xa=ya?ya.documentMode:void 0;if(xa>parseFloat(J)){I=String(xa);break a}}I=J}var ia=I,M={},za={},Aa=function(){return za[9]||(za[9]=G&&document.documentMode&&document.documentMode>=9)};!G||Aa();if(H||G)if(!G||!Aa())H&&(M["1.9.1"]||(M["1.9.1"]=ja("1.9.1")>=0));G&&(M["9"]||(M["9"]=ja("9")>=0));w("");var N=function(a){return!!a&&typeof a=="function"&&!!a.call},Ba=function(a,b){if(!(arguments.length<2))for(var c=1,d=arguments.length;c<d;++c)a.push(arguments[c])};function O(a){return typeof encodeURIComponent=="function"?encodeURIComponent(a):escape(a)}function Ca(a,b){a.attachEvent?a.attachEvent("onload",b):a.addEventListener&&a.addEventListener("load",b,i)};aa("google_protectAndRun",function(a,b,c){a=l(b,k,a);b=window.onerror;window.onerror=a;try{c()}catch(d){var c=d.toString(),e="";d.fileName&&(e=d.fileName);var f=-1;if(d.lineNumber)f=d.lineNumber;if(!a(c,e,f))throw d;}window.onerror=b});
aa("google_handleError",function(a,b,c,d){if(Math.random()<0.01)a=["http://",sa(),"/pagead/gen_204","?id=jserror","&jscb=",na?1:0,"&jscd=",oa?1:0,"&context=",O(a),"&msg=",O(b),"&file=",O(c),"&line=",O(d.toString()),"&url=",O(ka.URL.substring(0,512)),"&ref=",O(ka.referrer.substring(0,512))],a.push(["&client=",O(t.google_ad_client),"&format=",O(t.google_ad_format),"&slotname=",O(t.google_ad_slot),"&output=",O(t.google_ad_output),"&ad_type=",O(t.google_ad_type)].join("")),a=a.join(""),t.google_image_requests||
(t.google_image_requests=[]),b=t.document.createElement("img"),b.src=a,t.google_image_requests.push(b);return!pa});var Da=function(a){try{var b=a.google_test;a.google_test=!b;if(a.google_test===!b)return a.google_test=b,g}catch(c){}return i},Ea=h,Fa=function(){if(!Ea){for(var a=window;a!=a.parent&&Da(a.parent);)a=a.parent;Ea=a}return Ea};var P,Q=function(a){this.c=[];this.a=a||window;this.b=0;this.d=h},Ga=function(a,b){this.l=a;this.i=b};Q.prototype.n=function(a,b){this.b==0&&this.c.length==0&&(!b||b==window)?(this.b=2,this.g(new Ga(a,window))):this.h(a,b)};Q.prototype.h=function(a,b){this.c.push(new Ga(a,b||this.a));Ha(this)};Q.prototype.o=function(a){this.b=1;if(a)this.d=this.a.setTimeout(l(this.f,this),a)};Q.prototype.f=function(){if(this.b==1){if(this.d!=h)this.a.clearTimeout(this.d),this.d=h;this.b=0}Ha(this)};
Q.prototype.p=function(){return g};Q.prototype.nq=Q.prototype.n;Q.prototype.nqa=Q.prototype.h;Q.prototype.al=Q.prototype.o;Q.prototype.rl=Q.prototype.f;Q.prototype.sz=Q.prototype.p;var Ha=function(a){a.a.setTimeout(l(a.m,a),0)};Q.prototype.m=function(){if(this.b==0&&this.c.length){var a=this.c.shift();this.b=2;a.i.setTimeout(l(this.g,this,a),0);Ha(this)}};Q.prototype.g=function(a){this.b=0;a.l()};
var Ia=function(a){try{return a.sz()}catch(b){return i}},Ka=function(a){return!!a&&(typeof a=="object"||typeof a=="function")&&Ia(a)&&N(a.nq)&&N(a.nqa)&&N(a.al)&&N(a.rl)},La=function(){if(P&&Ia(P))return P;var a=Fa(),b=a.google_jobrunner;return Ka(b)?P=b:a.google_jobrunner=P=new Q(a)},Ma=function(a,b){La().nq(a,b)},Na=function(a,b){La().nqa(a,b)};var Oa=/MSIE [2-7]|PlayStation|Gecko\/20090226/i,Pa=/Android|Opera/,Qa=function(){var a=S,b=T.google_ad_width,c=T.google_ad_height,d=["<iframe"],e;for(e in a)a.hasOwnProperty(e)&&Ba(d,e+"="+a[e]);d.push('style="left:0;position:absolute;top:0;"');d.push("></iframe>");b="border:none;height:"+c+"px;margin:0;padding:0;position:relative;visibility:visible;width:"+b+"px";return['<ins style="display:inline-table;',b,'"><ins id="',a.id+"_anchor",'" style="display:block;',b,'">',d.join(" "),"</ins></ins>"].join("")};var Ra=function(){},Ta=function(a,b,c){switch(typeof b){case "string":Sa(b,c);break;case "number":c.push(isFinite(b)&&!isNaN(b)?b:"null");break;case "boolean":c.push(b);break;case "undefined":c.push("null");break;case "object":if(b==h){c.push("null");break}if(b instanceof Array){var d=b.length;c.push("[");for(var e="",f=0;f<d;f++)c.push(e),Ta(a,b[f],c),e=",";c.push("]");break}c.push("{");d="";for(e in b)b.hasOwnProperty(e)&&(f=b[e],typeof f!="function"&&(c.push(d),Sa(e,c),c.push(":"),Ta(a,f,c),d=
","));c.push("}");break;case "function":break;default:throw Error("Unknown type: "+typeof b);}},Ua={'"':'\\"',"\\":"\\\\","/":"\\/","\u0008":"\\b","\u000c":"\\f","\n":"\\n","\r":"\\r","\t":"\\t","\u000b":"\\u000b"},Va=/\uffff/.test("\uffff")?/[\\\"\x00-\x1f\x7f-\uffff]/g:/[\\\"\x00-\x1f\x7f-\xff]/g,Sa=function(a,b){b.push('"');b.push(a.replace(Va,function(a){if(a in Ua)return Ua[a];var b=a.charCodeAt(0),e="\\u";b<16?e+="000":b<256?e+="00":b<4096&&(e+="0");return Ua[a]=e+b.toString(16)}));b.push('"')};var U="google_ad_block,google_ad_channel,google_ad_client,google_ad_format,google_ad_height,google_ad_host,google_ad_host_channel,google_ad_host_tier_id,google_ad_output,google_ad_override,google_ad_region,google_ad_section,google_ad_slot,google_ad_type,google_ad_width,google_adtest,google_allow_expandable_ads,google_alternate_ad_url,google_alternate_color,google_analytics_domain_name,google_analytics_uacct,google_bid,google_city,google_color_bg,google_color_border,google_color_line,google_color_link,google_color_text,google_color_url,google_container_id,google_contents,google_country,google_cpm,google_ctr_threshold,google_cust_age,google_cust_ch,google_cust_gender,google_cust_id,google_cust_interests,google_cust_job,google_cust_l,google_cust_lh,google_cust_u_url,google_disable_video_autoplay,google_ed,google_eids,google_enable_ose,google_encoding,google_font_face,google_font_size,google_frame_id,google_gl,google_hints,google_image_size,google_kw,google_kw_type,google_language,google_max_num_ads,google_max_radlink_len,google_num_radlinks,google_num_radlinks_per_unit,google_num_slots_to_rotate,google_only_ads_with_video,google_only_pyv_ads,google_only_userchoice_ads,google_override_format,google_page_url,google_previous_watch,google_previous_searches,google_referrer_url,google_region,google_reuse_colors,google_rl_dest_url,google_rl_filtering,google_rl_mode,google_rt,google_safe,google_scs,google_skip,google_tag_info,google_targeting,google_tfs,google_tl,google_ui_features,google_ui_version,google_video_doc_id,google_video_product_type,google_with_pyv_ads".split(",");var V=function(a){this.a=a;a.google_iframe_oncopy||(a.google_iframe_oncopy={handlers:{},log:[],img:Math.random()<0.01?[]:h});this.e=a.google_iframe_oncopy;a.setTimeout(l(this.k,this),3E4)},Wa;var W="var i=this.id,s=window.google_iframe_oncopy,H=s&&s.handlers,h=H&&H[i],w=this.contentWindow,d;try{d=w.document}catch(e){}if(h&&d&&(!d.body||!d.body.firstChild)){if(h.call){i+='.call';setTimeout(h,0)}else if(h.match){i+='.nav';w.location.replace(h)}s.log&&s.log.push(i)}";
/[&<>\"]/.test(W)&&(W.indexOf("&")!=-1&&(W=W.replace(da,"&amp;")),W.indexOf("<")!=-1&&(W=W.replace(ea,"&lt;")),W.indexOf(">")!=-1&&(W=W.replace(fa,"&gt;")),W.indexOf('"')!=-1&&(W=W.replace(ha,"&quot;")));Wa=W;V.prototype.set=function(a,b){this.e.handlers[a]=b;this.a.addEventListener&&this.a.addEventListener("load",l(this.j,this,a),i)};V.prototype.j=function(a){var a=this.a.document.getElementById(a),b=a.contentWindow.document;if(a.onload&&b&&(!b.body||!b.body.firstChild))a.onload()};
V.prototype.k=function(){if(this.e.img){var a=this.e.log,b=this.a.document;if(a.length)b=["http://",sa(),"/pagead/gen_204?id=iframecopy&log=",O(a.join("-")),"&url=",O(b.URL.substring(0,512)),"&ref=",O(b.referrer.substring(0,512))].join(""),a.length=0,a=new Image,this.e.img.push(a),a.src=b}};var Xa=function(){var a="script",b=w("");return["<",a,' src="',[qa?"https":"http","://",b,"/pagead/js/r20111102/r20110914/show_ads_impl.js"].join(""),'"></',a,">"].join("")},Ya=function(a,b,c,d){return function(){var e=i;d&&La().al(3E4);try{var f;try{f=!!a.document.getElementById(b).contentWindow.document}catch(ra){f=i}if(f){var u=
a.document.getElementById(b).contentWindow,n=u.document;if(!n.body||!n.body.firstChild)n.open(),u.google_async_iframe_close=g,n.write(c)}else{var o=a.document.getElementById(b).contentWindow,p;f=c;f=String(f);if(f.quote)p=f.quote();else{u=['"'];for(n=0;n<f.length;n++){var R=f.charAt(n),Ja=R.charCodeAt(0),pb=u,qb=n+1,ga;if(!(ga=m[R])){var D;if(Ja>31&&Ja<127)D=R;else{var r=R;if(r in q)D=q[r];else if(r in m)D=q[r]=m[r];else{var x=r,y=r.charCodeAt(0);if(y>31&&y<127)x=r;else{if(y<256){if(x="\\x",y<16||
y>256)x+="0"}else x="\\u",y<4096&&(x+="0");x+=y.toString(16).toUpperCase()}D=q[r]=x}}ga=D}pb[qb]=ga}u.push('"');p=u.join("")}o.location.replace("javascript:"+p)}e=g}catch(xb){o=Fa().google_jobrunner,Ka(o)&&o.rl()}e&&(new V(a)).set(b,Ya(a,b,c,i))}};window.google_loader_used=g;(function(a){if(!("google_onload_fired"in a))a.google_onload_fired=i,Ca(a,function(){a.google_onload_fired=g})})(window);if(!window.google_loader_experiment){var Za;a:{var $a=["async_bad_black","block_bad_black"];if(!(Math.random()<1.0E-4)){var ab=Math.random();if(ab<ma){Za=$a[Math.floor(ab/ma*$a.length)];break a}}Za=h}window.google_loader_experiment=Za||""||"launch"}var bb;
a:{try{if(window.google_enable_async!==g&&window.google_loader_experiment=="blockodd"&&window.top.location.hostname.length%2==1){bb=i;break a}}catch(cb){}bb=g}var db;if(db=bb){var eb;if(window.google_enable_async===i)eb=0;else{var fb=navigator.userAgent,gb=window.google_loader_experiment;eb=(Oa.test(fb)?i:Pa.test(fb)?gb=="async_bad_black":g)&&!window.google_container_id&&(!window.google_ad_output||window.google_ad_output=="html")}db=eb}
if(db){var hb=window;hb.google_unique_id?++hb.google_unique_id:hb.google_unique_id=1;var X=window;if(!X.google_slot_list||!X.google_slot_list.push)X.google_slot_list=[];X.google_slot_list.push([X.google_ad_slot||"",X.google_ad_format||"",X.google_ad_width||"",X.google_ad_height||""].join("."));for(var Y=window,_script$$inline_169="script",Z,T=Y,S={allowtransparency:'"true"',frameborder:'"0"',height:'"'+Y.google_ad_height+'"',hspace:'"0"',marginwidth:'"0"',marginheight:'"0"',onload:'"'+Wa+'"',scrolling:'"no"',
vspace:'"0"',width:'"'+Y.google_ad_width+'"'},ib=T.document,$=S.id,jb=0;!$||T.document.getElementById($);)$="aswift_"+jb++;S.id=$;S.name=$;ib.write(Qa());Z=$;var kb;Y.google_page_url&&(Y.google_page_url=String(Y.google_page_url));for(var lb=[],mb=0,nb=U.length;mb<nb;mb++){var ob=U[mb];if(Y[ob]!=h){var rb;try{var sb=[];Ta(new Ra,Y[ob],sb);rb=sb.join("")}catch(tb){}rb&&Ba(lb,ob,"=",rb,";")}}kb=lb.join("");for(var ub=0,vb=U.length;ub<vb;ub++)Y[U[ub]]=h;var wb=(new Date).getTime(),yb=window.google_loader_experiment,
zb=["<!doctype html><html><body><",_script$$inline_169,">",kb,"google_show_ads_impl=true;google_unique_id=",Y.google_unique_id,';google_async_iframe_id="',Z,'";google_start_time=',j,";",yb?'google_loader_experiment="'+yb+'";':"","google_bpp=",wb>j?wb-j:1,";</",_script$$inline_169,">",Xa(),"</body></html>"].join("");(Y.document.getElementById(Z)?Ma:Na)(Ya(Y,Z,zb,g))}else window.q=j,document.write(Xa());})();