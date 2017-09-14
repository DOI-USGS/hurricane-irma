var prcpColors;
var prcpTimes;
var pt;
var svg

var running = false;
var interval = undefined;
var intervalLength = 160;
var timestep = 1;
var filename;

if (window.innerWidth > window.innerHeight) {
  filename = 'images/hurricane-map-landscape.svg';
}
else {
  filename = 'images/hurricane-map-portrait.svg';
}

var fetchSvg = $.ajax({
  url: filename,
  dataType: 'html'
});

var fetchPrcpColors = $.ajax({url:'js/precip-colors.json', dataType: 'json'});
var fetchPrcpTimes = $.ajax({url:'js/times.json', dataType: 'json'});

var animatePrcp = function(timestep, $currentStormDot) {
  prcpColors.forEach(function(color, index) {
    var bin = index + 1;
    var $prcpBin = $('.p-' + timestep + '-' + bin);
    var stormX = $currentStormDot.attr('cx');
    var stormY = $currentStormDot.attr('cy');
    $prcpBin.css("fill", color);
    
    if ($currentStormDot){
      $currentStormDot.data('cx', stormX);
      $currentStormDot.data('cy', stormY);
    }
  });

  $('.nwis-dot').css('fill', '#73877B').css('stroke', "#385F71");
  $('.f-' + timestep).css('fill', '#ff7000');
  $('#timestamp-text').html(prcpTimes.times[timestep - 1]);

  var darkWidth = (timestep+1)/prcpTimes.times.length;
  $('#spark-light-mask').attr('x', darkWidth).attr('width', 1 - darkWidth);
  $('#spark-full-mask').attr('width', darkWidth);
  $('#flood-light-mask').attr('x', darkWidth).attr('width', 1 - darkWidth);
  $('#flood-full-mask').attr('width', darkWidth);
};

var play = function() {
  var button = $('#playButton');
  if (!running) {
    running = true;
    ga('send', 'event', 'figure', 'user pressed play');
    button.css('display', 'none');
    interval = setInterval(function() {
      
      var $stormDot = $('.storm-dot');
      $stormDot.css("opacity", "0").css("transform", "scale(0.1");
      var $currentStormDot = $('#storm-' + timestep);
      $currentStormDot.css('opacity', '1.0').css('transform', 'scale(1)');
      
      if (timestep < prcpTimes.times.length) {
        animatePrcp(timestep, $currentStormDot);
        timestep++;
      } else {
        timestep = 1;
        clearInterval(interval);
        running = false;
        button.css('display', 'block');
      }
    }, intervalLength);
  }  
}
var pause = function() {
  var button = $('#playButton');
  if (running) {
    clearInterval(interval);
    running = false;
    button.css('display', 'block');
    ga('send', 'event', 'figure', 'user pressed pause');
  }
};
$('document').ready(function() {
  fetchSvg.done(function(data) {
    $('#map-figure figure').html(data);
    $('#map-figure svg').ready(function() {
        $.when(fetchPrcpColors, fetchPrcpTimes).done(function() {
          prcpTimes = fetchPrcpTimes.responseJSON;
          prcpColors = fetchPrcpColors.responseJSON;
          svg = document.querySelector("svg");
          pt = svg.createSVGPoint();
          $('.viz-pause').on('click', function(){
            pause();
          });
          play();
        });
    });
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
    url = 'http://waterdata.usgs.gov/nwis/uv?site_no=' + id;
    ga('send', 'event', 'outbound', 'click', url);
    window.open(url, '_blank');
  }
}

function setBold(id){
  var className = id.split('-')[0];
  $('#' + id).removeClass(className).addClass(className + '-bold');
  $('#storm_sites circle[id=' + id + ']').removeClass(className + '-bold').addClass(className)
}
function setNormal(id){
  var className = id.split('-')[0];
  $('#' + id).removeClass(className + '-bold').addClass(className);
}