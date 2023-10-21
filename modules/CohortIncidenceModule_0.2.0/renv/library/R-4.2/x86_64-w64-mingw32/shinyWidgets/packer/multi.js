/*! For license information please see multi.js.LICENSE.txt */
(()=>{"use strict";var e={3822:(e,t,r)=>{r.d(t,{Z:()=>l});var n=r(8081),a=r.n(n),i=r(3645),o=r.n(i)()(a());o.push([e.id,".multi-wrapper{border:1px solid #ccc;border-radius:3px;width:100%}.multi-wrapper .non-selected-wrapper,.multi-wrapper .selected-wrapper{box-sizing:border-box;display:inline-block;height:200px;overflow-y:scroll;padding:10px;vertical-align:top;width:50%}.multi-wrapper .non-selected-wrapper{background:#fafafa;border-right:1px solid #ccc}.multi-wrapper .selected-wrapper{background:#fff}.multi-wrapper .header{color:#4f4f4f;cursor:default;font-weight:700;margin-bottom:5px;padding:5px 10px}.multi-wrapper .item{cursor:pointer;display:block;padding:5px 10px}.multi-wrapper .item:hover{background:#ececec;border-radius:2px}.multi-wrapper .item-group{padding:5px 10px}.multi-wrapper .item-group .group-label{display:block;font-size:.875rem;opacity:.5;padding:5px 0}.multi-wrapper .search-input{border:0;border-bottom:1px solid #ccc;border-radius:0;display:block;font-size:1em;margin:0;outline:0;padding:10px 20px;width:100%;box-sizing:border-box}.multi-wrapper .non-selected-wrapper .item.selected{opacity:.5}.multi-wrapper .non-selected-wrapper .item.disabled,.multi-wrapper .selected-wrapper .item.disabled{opacity:.5;text-decoration:line-through}.multi-wrapper .non-selected-wrapper .item.disabled:hover,.multi-wrapper .selected-wrapper .item.disabled:hover{background:inherit;cursor:inherit}",""]);const l=o},1629:(e,t,r)=>{r.d(t,{Z:()=>l});var n=r(8081),a=r.n(n),i=r(3645),o=r.n(i)()(a());o.push([e.id,"a.item:focus, a.item:hover {\n  text-decoration: none;\n}\n",""]);const l=o},3645:e=>{e.exports=function(e){var t=[];return t.toString=function(){return this.map((function(t){var r="",n=void 0!==t[5];return t[4]&&(r+="@supports (".concat(t[4],") {")),t[2]&&(r+="@media ".concat(t[2]," {")),n&&(r+="@layer".concat(t[5].length>0?" ".concat(t[5]):""," {")),r+=e(t),n&&(r+="}"),t[2]&&(r+="}"),t[4]&&(r+="}"),r})).join("")},t.i=function(e,r,n,a,i){"string"==typeof e&&(e=[[null,e,void 0]]);var o={};if(n)for(var l=0;l<this.length;l++){var d=this[l][0];null!=d&&(o[d]=!0)}for(var s=0;s<e.length;s++){var c=[].concat(e[s]);n&&o[c[0]]||(void 0!==i&&(void 0===c[5]||(c[1]="@layer".concat(c[5].length>0?" ".concat(c[5]):""," {").concat(c[1],"}")),c[5]=i),r&&(c[2]?(c[1]="@media ".concat(c[2]," {").concat(c[1],"}"),c[2]=r):c[2]=r),a&&(c[4]?(c[1]="@supports (".concat(c[4],") {").concat(c[1],"}"),c[4]=a):c[4]="".concat(a)),t.push(c))}},t}},8081:e=>{e.exports=function(e){return e[1]}},3379:e=>{var t=[];function r(e){for(var r=-1,n=0;n<t.length;n++)if(t[n].identifier===e){r=n;break}return r}function n(e,n){for(var i={},o=[],l=0;l<e.length;l++){var d=e[l],s=n.base?d[0]+n.base:d[0],c=i[s]||0,p="".concat(s," ").concat(c);i[s]=c+1;var u=r(p),f={css:d[1],media:d[2],sourceMap:d[3],supports:d[4],layer:d[5]};if(-1!==u)t[u].references++,t[u].updater(f);else{var m=a(f,n);n.byIndex=l,t.splice(l,0,{identifier:p,updater:m,references:1})}o.push(p)}return o}function a(e,t){var r=t.domAPI(t);return r.update(e),function(t){if(t){if(t.css===e.css&&t.media===e.media&&t.sourceMap===e.sourceMap&&t.supports===e.supports&&t.layer===e.layer)return;r.update(e=t)}else r.remove()}}e.exports=function(e,a){var i=n(e=e||[],a=a||{});return function(e){e=e||[];for(var o=0;o<i.length;o++){var l=r(i[o]);t[l].references--}for(var d=n(e,a),s=0;s<i.length;s++){var c=r(i[s]);0===t[c].references&&(t[c].updater(),t.splice(c,1))}i=d}}},569:e=>{var t={};e.exports=function(e,r){var n=function(e){if(void 0===t[e]){var r=document.querySelector(e);if(window.HTMLIFrameElement&&r instanceof window.HTMLIFrameElement)try{r=r.contentDocument.head}catch(e){r=null}t[e]=r}return t[e]}(e);if(!n)throw new Error("Couldn't find a style target. This probably means that the value for the 'insert' parameter is invalid.");n.appendChild(r)}},9216:e=>{e.exports=function(e){var t=document.createElement("style");return e.setAttributes(t,e.attributes),e.insert(t,e.options),t}},3565:(e,t,r)=>{e.exports=function(e){var t=r.nc;t&&e.setAttribute("nonce",t)}},7795:e=>{e.exports=function(e){var t=e.insertStyleElement(e);return{update:function(r){!function(e,t,r){var n="";r.supports&&(n+="@supports (".concat(r.supports,") {")),r.media&&(n+="@media ".concat(r.media," {"));var a=void 0!==r.layer;a&&(n+="@layer".concat(r.layer.length>0?" ".concat(r.layer):""," {")),n+=r.css,a&&(n+="}"),r.media&&(n+="}"),r.supports&&(n+="}");var i=r.sourceMap;i&&"undefined"!=typeof btoa&&(n+="\n/*# sourceMappingURL=data:application/json;base64,".concat(btoa(unescape(encodeURIComponent(JSON.stringify(i))))," */")),t.styleTagTransform(n,e,t.options)}(t,e,r)},remove:function(){!function(e){if(null===e.parentNode)return!1;e.parentNode.removeChild(e)}(t)}}}},4589:e=>{e.exports=function(e,t){if(t.styleSheet)t.styleSheet.cssText=e;else{for(;t.firstChild;)t.removeChild(t.firstChild);t.appendChild(document.createTextNode(e))}}}},t={};function r(n){var a=t[n];if(void 0!==a)return a.exports;var i=t[n]={id:n,exports:{}};return e[n](i,i.exports,r),i.exports}r.n=e=>{var t=e&&e.__esModule?()=>e.default:()=>e;return r.d(t,{a:t}),t},r.d=(e,t)=>{for(var n in t)r.o(t,n)&&!r.o(e,n)&&Object.defineProperty(e,n,{enumerable:!0,get:t[n]})},r.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t),(()=>{const e=jQuery;var t=r.n(e);Shiny;var n,a,i=(n=function(e,t,r){var n=e.options[t.target.getAttribute("multi-index")];if(!n.disabled){n.selected=!n.selected;var a,i,o=r.limit;if(o>-1){for(var l=0,d=0;d<e.options.length;d++)e.options[d].selected&&l++;if(l===o)for(this.disabled_limit=!0,"function"==typeof r.limit_reached&&r.limit_reached(),d=0;d<e.options.length;d++)(s=e.options[d]).selected||s.setAttribute("disabled",!0);else if(this.disabled_limit){for(d=0;d<e.options.length;d++){var s;"false"===(s=e.options[d]).getAttribute("data-origin-disabled")&&s.removeAttribute("disabled")}this.disabled_limit=!1}}a=e,(i=document.createEvent("HTMLEvents")).initEvent("change",!1,!0),a.dispatchEvent(i)}},a=function(e,t){if(e.wrapper.selected.innerHTML="",e.wrapper.non_selected.innerHTML="",t.non_selected_header&&t.selected_header){var r=document.createElement("div"),n=document.createElement("div");r.className="header",n.className="header",r.innerText=t.non_selected_header,n.innerText=t.selected_header,e.wrapper.non_selected.appendChild(r),e.wrapper.selected.appendChild(n)}if(e.wrapper.search)var a=e.wrapper.search.value;for(var i=null,o=null,l=0;l<e.options.length;l++){var d=e.options[l],s=d.value,c=d.textContent||d.innerText,p=document.createElement("a");if(p.tabIndex=0,p.className="item",p.innerHTML=c,p.setAttribute("role","button"),p.setAttribute("data-value",s),p.setAttribute("multi-index",l),d.disabled&&(p.className+=" disabled"),d.selected){p.className+=" selected";var u=p.cloneNode(!0);e.wrapper.selected.appendChild(u)}if("OPTGROUP"==d.parentNode.nodeName&&d.parentNode!=o){if(o=d.parentNode,(i=document.createElement("div")).className="item-group",d.parentNode.label){var f=document.createElement("span");f.innerHTML=d.parentNode.label,f.className="group-label",i.appendChild(f)}e.wrapper.non_selected.appendChild(i)}d.parentNode==e&&(i=null,o=null),(!a||a&&c.toLowerCase().indexOf(a.toLowerCase())>-1)&&(null!=i?i.appendChild(p):e.wrapper.non_selected.appendChild(p))}},function(e,t){if((t=void 0!==t?t:{}).enable_search=void 0===t.enable_search||t.enable_search,t.search_placeholder=void 0!==t.search_placeholder?t.search_placeholder:"Search...",t.non_selected_header=void 0!==t.non_selected_header?t.non_selected_header:null,t.selected_header=void 0!==t.selected_header?t.selected_header:null,t.limit=void 0!==t.limit?parseInt(t.limit):-1,isNaN(t.limit)&&(t.limit=-1),null==e.dataset.multijs&&"SELECT"==e.nodeName&&e.multiple){e.style.display="none",e.setAttribute("data-multijs",!0);var r=document.createElement("div");if(r.className="multi-wrapper",t.enable_search){var i=document.createElement("input");i.className="search-input",i.type="text",i.setAttribute("placeholder",t.search_placeholder),i.addEventListener("input",(function(){a(e,t)})),r.appendChild(i),r.search=i}var o=document.createElement("div");o.className="non-selected-wrapper";var l=document.createElement("div");l.className="selected-wrapper",r.addEventListener("click",(function(r){r.target.getAttribute("multi-index")&&n(e,r,t)})),r.addEventListener("keypress",(function(r){var a=32===r.keyCode||13===r.keyCode;r.target.getAttribute("multi-index")&&a&&(r.preventDefault(),n(e,r,t))})),r.appendChild(o),r.appendChild(l),r.non_selected=o,r.selected=l,e.wrapper=r,e.parentNode.insertBefore(r,e.nextSibling);for(var d=0;d<e.options.length;d++){var s=e.options[d];s.setAttribute("data-origin-disabled",s.disabled)}a(e,t),e.addEventListener("change",(function(){a(e,t)}))}});"undefined"!=typeof jQuery&&function(e){e.fn.multi=function(t){return t=void 0!==t?t:{},this.each((function(){var r=e(this);i(r.get(0),t)}))}}(jQuery);var o=r(3379),l=r.n(o),d=r(7795),s=r.n(d),c=r(569),p=r.n(c),u=r(3565),f=r.n(u),m=r(9216),h=r.n(m),v=r(4589),b=r.n(v),g=r(3822),y={};y.styleTagTransform=b(),y.setAttributes=f(),y.insert=p().bind(null,"head"),y.domAPI=s(),y.insertStyleElement=h(),l()(g.Z,y),g.Z&&g.Z.locals&&g.Z.locals;var w=r(1629),x={};x.styleTagTransform=b(),x.setAttributes=f(),x.insert=p().bind(null,"head"),x.domAPI=s(),x.insertStyleElement=h(),l()(w.Z,x),w.Z&&w.Z.locals&&w.Z.locals;var _=new Shiny.InputBinding;t().extend(_,{initialize:function(e){var r=t()(e).parent().find('script[data-for="'+Shiny.$escape(e.id)+'"]');r=JSON.parse(r.html()),t()(e).multi(r),t()(e).trigger("change")},find:function(e){return t()(e).find(".multijs")},getId:function(e){return e.id},getValue:function(e){return t()(e).val()},setValue:function(e,r){t()(e).val(r),t()(e).multi(),t()(e).trigger("change")},getState:function(e){for(var r=new Array(e.length),n=0;n<e.length;n++)r[n]={value:e[n].value,label:e[n].label};return{label:t()(e).parent().find('label[for="'+Shiny.$escape(e.id)+'"]').text(),value:this.getValue(e),options:r}},receiveMessage:function(e,r){var n=t()(e);r.hasOwnProperty("options")&&n.empty().append(r.options),r.hasOwnProperty("value")&&this.setValue(e,r.value),r.hasOwnProperty("label")&&t()(e).parent().parent().find('label[for="'+Shiny.$escape(e.id)+'"]').text(r.label);var a=new Event("change");t()(e).multi(),n.get(0).dispatchEvent(a),n.trigger("change")},subscribe:function(e,r){t()(e).on("change",(function(e){r()}))},unsubscribe:function(e){t()(e).off(".multiInputBinding")}}),Shiny.inputBindings.register(_,"shinyWidgets.multiInput")})()})();