// Is simulation running
var ddc_running = false;

// Is simulation running in loop mode
var ddc_looping = false;

// actual line number
var linenr = 0;

// all pos chars
var positions = [
	["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O"],
	["A", "B", "C", "D", "E"],
	["F", "G", "H", "I", "J"],
	["K", "L", "M", "N", "O"]
];

// Start the simulation in endless loop
function ddc_loop() {
	ddc_running = true;
	ddc_looping = true;
	linenr = 0;
	ddc_run();
}

// Set up event handlers
function ddc_init() {
	$("start").onclick = function() {ddc_start();};
	$("stop").onclick = function() {ddc_stop();};
	$("loop").onclick = function() {ddc_loop();};
	ddc_fade_color("0", "#000000", 1000);
}


// Start the simulation
function ddc_start() {
	ddc_running = true;
	ddc_looping = false;
	linenr = 0;
	ddc_run();
}

function ddc_run() {
	code = $("anim").value.toUpperCase().split("\n");
	
	while(code[linenr] != null) {
		
		if(!ddc_running) return;
		
		// not needed chars filtering
		var line = code[linenr].replace("\r");
		
		// set next line number (actual line is already fetched)
		linenr++;
		
		// ignore empty lines
		if(line.length == 0) continue;
		
		// ignore comments
		if(line.substr(0, 1) == "#") continue;
		if(line.substr(0, 2) == "//") continue;
		
		// wait some time
		if(line.substr(0, 1) == "W") {
			// check valid time
			if(line.substr(1, 5).match(/[0-9]{1,5}/)) {
				window.setTimeout("ddc_run();", line.substr(1, 5).match(/[0-9]{1,5}/));
				return;
			}
		}
		
		// fade or set
		if(matches = line.match(/([CF])\s+([FB]?[123A-O]{1}|0|[FB]?ALL|[FB]?[123]?RND|RND1|RND2)\s+(#[0-9A-F]{6}|RND1|RND2|RND3|RGB1|RGB2)(?:\s+([0-9]{1,4}|RND))?/)) {
			
			// type: fade or set?
			type = matches[1];
			
			// if fade: 3 parameter needed
			if ( type == "F" && matches[4] == "" )
			{
				continue;
			}
			
			// position
			pos = "";
			temp_pos = matches[2];
			
			// Front or Back?
			first = temp_pos.substr(0, 1);
			if ( first == "F" || first == "B" ) {
				pos += first;
				temp_pos = temp_pos.substr(1);
			}
			// Random Front/Back
			else {
				pos += "FB".substr(rand(0, 1), 1);
			}
			
			mode = "0";
			switch ( temp_pos ) {
				// Only one C?
				case "1RND":
				case "2RND":
				case "3RND":
					mode = temp_pos.substr(0, 1);
				// All Cs
				case "RND":
					pos += positions[mode][rand(0, positions[mode].length-1)];
					break;
				
				case "RND1": 
					// make position
					pos += "ABCDEFGHIJKLMNO".substr(rand(0, 14), 1);
					break;
					
				case "RND2": 
					// make position including 0
					pos += "ABCDEFGHIJKLMNO0".substr(rand(0, 15), 1);
					break;
					
				default:
					pos = temp_pos;
					break;
			}
			
			// make random color
			switch ( matches[3] ) {
				case "RND1":
					color = "#";
					for(i = 0; i < 6; i++)
						color += "0123456789ABCDEF".substr(rand(0, 15), 1);
					break;
					
				case "RND2":
					color = "#";
					for(i = 0; i < 3; i++) {
						temp = rand(180, 255);
						color += "0123456789ABCDEF".substr((temp - temp % 16) / 16, 1);
						color += "0123456789ABCDEF".substr(temp % 16, 1);
					}
					break;
					
				case "RND3":
					color = "#";
					for(i = 0; i < 3; i++) {
						temp = rand(180, 256);
						color += "0123456789ABCDEF".substr((temp - temp % 16) / 16 % 16, 1);
						color += "0123456789ABCDEF".substr(temp % 16, 1);
					}
					break;
					
				case "RGB1":
					color = "#";
					temp = rand(0, 2);
					color += "FF000000FF000000FF".substr(temp * 6, 6);
					break;
					
				case "RGB2":
					color = "#";
					temp = rand(0, 5);
					color += "FF000000FF000000FFFFFF0000FFFFFF00FF".substr(temp * 6, 6);
					break;

				default:
					color = matches[3];
					break;
			}
			
			// make random speed
			speed = matches[4];
			if(speed == "RND") {
				speed = rand(0, 1000);
			}
			
			// temp: set color direct
			(type == "C" ) ? ddc_set_color(pos, color) : ddc_fade_color(pos, color, speed);
		}
	}
	linenr = 0;
	if(ddc_looping) {
		window.setTimeout("ddc_run();", 0);
	}
}

// fade color
function ddc_fade_color(pos, color, speed) {
	var ms = Math.ceil(1 / speed * 117 * 1000);
	var start = ($(pos) == null ? $("FA").style.backgroundColor : $(pos).style.backgroundColor);
	if(start.length < 7) {
		start = "#FFFFFF";
	} else {
		colors = start.match(/rgb\(([0-9]{1,3}), ([0-9]{1,3}), ([0-9]{1,3})\)/);
		red = colors[1];
		green = colors[2];
		blue = color[3];
		
		start = "#";
		start += "0123456789ABCDEF".substr((red - red % 16) / 16 % 16, 1);
		start += "0123456789ABCDEF".substr(red % 16, 1);
		start += "0123456789ABCDEF".substr((green - green % 16) / 16 % 16, 1);
		start += "0123456789ABCDEF".substr(green % 16, 1);
		start += "0123456789ABCDEF".substr((blue - blue % 16) / 16 % 16, 1);
		start += "0123456789ABCDEF".substr(blue % 16, 1);
	}
	ddc_fade(pos, start, color, ms);
}

// internal color fader
function ddc_fade(pos, start, end, ms) {
	var stepping = 20;

	red_start = "0123456789ABCDEF".indexOf(start.substr(1, 1)) * 16 + "0123456789ABCDEF".indexOf(start.substr(2, 1));
	green_start = "0123456789ABCDEF".indexOf(start.substr(3, 1)) * 16 + "0123456789ABCDEF".indexOf(start.substr(4, 1));
	blue_start = "0123456789ABCDEF".indexOf(start.substr(5, 1)) * 16 + "0123456789ABCDEF".indexOf(start.substr(6, 1));
	
	red_end = "0123456789ABCDEF".indexOf(end.substr(1, 1)) * 16 + "0123456789ABCDEF".indexOf(end.substr(2, 1));
	green_end = "0123456789ABCDEF".indexOf(end.substr(3, 1)) * 16 + "0123456789ABCDEF".indexOf(end.substr(4, 1));
	blue_end = "0123456789ABCDEF".indexOf(end.substr(5, 1)) * 16 + "0123456789ABCDEF".indexOf(end.substr(6, 1));

	red = Math.ceil(red_start + (red_end - red_start) / (ms / stepping));
	green = Math.ceil(green_start + (green_end - green_start) / (ms / stepping));
	blue = Math.ceil(blue_start + (blue_end - blue_start) / (ms / stepping));
	
	color = "#";
	color += "0123456789ABCDEF".substr((red - red % 16) / 16 % 16, 1);
	color += "0123456789ABCDEF".substr(red % 16, 1);
	color += "0123456789ABCDEF".substr((green - green % 16) / 16 % 16, 1);
	color += "0123456789ABCDEF".substr(green % 16, 1);
	color += "0123456789ABCDEF".substr((blue - blue % 16) / 16 % 16, 1);
	color += "0123456789ABCDEF".substr(blue % 16, 1);
	
	ms = ms - stepping;
	if(ms < 0) return;

	ddc_set_color(pos, color);
	//alert(start + "->" + color + "->" + end);
	
	window.setTimeout("ddc_fade('" + pos + "', '" + color + "', '" + end + "', " + ms + ");", stepping);
}


// set color to specific position
function ddc_set_color(pos, color) {
	if ( pos.constructor == Array ) {
		for each ( item in pos ) {
			if ( item == "0" ) {
				//    0    All front+back [deprecated]
				item = "ALL";
			}
			switch ( item.length ) {
				case 3: 
					//    ALL  All front+back
				case 1: 
					//    1    full C1 front+back
					//    2    full C2 front+back
					//    3    full C3 front+back
					//    A-O  front+back
					ddc_set_color(["F"+item, "B"+item], color);
					break;
					
				case 4: 
					//    FALL    All front
					//    BALL    All back
					ddc_set_color(positions[0].map(function(x) { return item.substr(0, 1)+x; }), color);
					break;
				case 2: 
					//    F%      front %
					//    B%      back %
					switch ( item.substr(1) ) {
						case "1":
							//    F1    full C1 front
							//    B1    full C1 back
						case "2":
							//    F2    full C2 front
							//    B2    full C2 back
						case "3":
							//    F3    full C3 front
							//    B3    full C3 back
							ddc_set_color(positions[item.substr(1)].map(function(x) { return item.substr(0, 1)+x; }), color);
							break;

						default:
							//    FA-FO    (front)
							//    BA-BO    (back)
							$(item).style.backgroundColor = color;
							break;
					}
					break;
			}
			
		}
	}
	else {
		ddc_set_color(new Array(pos), color);
	} 
}

// Stop the simulation
function ddc_stop() {
	ddc_running = false;
	ddc_looping = false;
}

// Shorter id function
function $(id) {
	return document.getElementById(id);
}

// Random int function
function rand(from, to) {
	return Math.ceil(Math.random() * (to - from + 1)) + from - 1;
}