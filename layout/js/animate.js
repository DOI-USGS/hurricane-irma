var prcpColors;
var prcpTimes;
var pt;
var svg

var running = false;
var interval = undefined;
var intervalLength = 160;
var timestep = 1;

var fetchPrcpColors = $.get("js/precip-colors.json").done(function(data) {
  prcpColors = data;
});
var fetchPrcpTimes = $.get("js/times.json").done(function(data) {
  prcpTimes = data;
});

var animatePrcp = function(timestep) {
  prcpColors.forEach(function(color, index) {
    var bin = index + 1;
    var $prcpBin = $('.p-' + timestep + '-' + bin);
    var $stormDot = $('.storm-dot');
    var $currentStormDot = $('#storm-' + timestep)

    $prcpBin.css("fill", color);
    
    $stormDot.css("opacity", "0").css("transform", "scale(0.1");
    
    if ($currentStormDot){
      $currentStormDot.css('opacity', '1.0').css('transform', 'scale(0.1)');
    }
  });

  $('#timestamp-text').html(prcpTimes.times[timestep - 1]);

  var darkWidth = timestep/prcpTimes.times.length;
  $('#spark-light-mask').attr('x', darkWidth).attr('width', 1 - darkWidth);
  $('#spark-full-mask').attr('width', darkWidth);
};

var playPause = function() {
  var button = $('#playButton');
  if (running) {
    clearInterval(interval);
    running = false;
    button.html("Play");
    ga('send', 'event', 'figure', 'user pressed pause');
  } else {
    running = true;
    ga('send', 'event', 'figure', 'user pressed play');
    button.html("Pause")
    interval = setInterval(function() {
      if (timestep < prcpTimes.times.length) {
        animatePrcp(timestep);
        timestep++;
      } else {
        timestep = 1;
        clearInterval(interval);
        running = false;
        button.html("Play");
      }
    }, intervalLength);
  }
};

$(document).ready(function() {
  $.when(fetchPrcpTimes, fetchPrcpColors)
    .done(function() {
      svg = document.querySelector("svg");
      pt = svg.createSVGPoint();  
    });
});


var hoverTimer = null;
var hoverDelay = 400; //ms
  
function hovertext(text, evt){
  var tooltip = document.getElementById("tooltip-text");
  var tooltip_bg = document.getElementById("tooltip-box");    
  var tool_pt = document.getElementById("tooltip-point");
  if (evt === undefined){
    if(hoverTimer) {
      clearTimeout(hoverTimer); //stop when off area
    }
    tooltip.firstChild.data = ' ';
    tooltip_bg.setAttribute("class","hidden");
    tooltip_bg.setAttribute("x",0);
    tool_pt.setAttribute("class","hidden");
  } else {
    pt = cursorPoint(evt);
    pt.x = Math.round(pt.x);
    pt.y = Math.round(pt.y);
    var svgWidth = Number(svg.getAttribute("viewBox").split(" ")[2]);
    tooltip.setAttribute("x",pt.x);
    tooltip.setAttribute("y",pt.y);
    tooltip.firstChild.data = text;
    var length = Math.round(tooltip.getComputedTextLength());
    if (pt.x - length/2 - 6 < 0){
      tooltip.setAttribute("x",length/2+6);
    } else if (pt.x + length/2 + 6 > svgWidth) {
      tooltip.setAttribute("x", svgWidth-length/2-6);
    }
    tool_pt.setAttribute("transform","translate("+pt.x+","+pt.y+")");
    tooltip_bg.setAttribute("x",tooltip.getAttribute("x")-length/2-6);
    tooltip_bg.setAttribute("y",pt.y-35);
    tooltip.setAttribute("class","shown");
    tooltip_bg.setAttribute("class","tooltip-box");
    tool_pt.setAttribute("class","tooltip-box");
    tooltip_bg.setAttribute("width", length+12);
    if(hoverTimer){
      clearTimeout(hoverTimer);
    }
    hoverTimer = setTimeout(function(){
      ga("send", "event", "figure", evt.target.id);
    }, hoverDelay);
  }
}
function cursorPoint(evt){  
  pt.x = evt.clientX; pt.y = evt.clientY;
  return pt.matrixTransform(svg.getScreenCTM().inverse());
}
function openNWIS(id, event){
 if( /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) ) {
   event.stopPropagation();
}else{
   vizlab.clicklink('http://waterdata.usgs.gov/nwis/uv?site_no='+id,'_blank');
  }
  
}
