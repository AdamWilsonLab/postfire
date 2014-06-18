// CapePointLANDSAT 

//  Specify destination and run name
var drawmap=true
var driveFolder="ee_ZA_output";
var run="r1"
var exportfiles=true
var verbose=true

// limit overall date range  (only dates in this range will be included)
var datestart=new Date("2010-01-01")  // default time zone is UTC
var datestop=new Date("2011-12-31")

// Identify LANDSAT collections to include in processing

// create array of all products to process
var prods =['LANDSAT/LT4_L1T_ANNUAL_GREENEST_TOA',
            'LANDSAT/LT5_L1T_ANNUAL_GREENEST_TOA',
            'LANDSAT/LE7_L1T_ANNUAL_GREENEST_TOA',
            'LANDSAT/LC8_L1T_ANNUAL_GREENEST_TOA'] 


// Get current date as string for file metadata
var currentdate = new Date();
var date= currentdate.getFullYear()+''+("0" + (currentdate.getMonth() + 1)).slice(-2)+''+currentdate.getDate();
print(date)


// zoom to Cape
centerMap(18.4,-34.2,11);

// Define a rectangular region to search landsat scenes
var studyArea = ee.Feature(ee.Geometry.Polygon([[18,-33.89], [18,-34.4], [18.5, -34.4],[18.5, -33.89], [18,-33.89]]));

// Create a mask using SRTM elevation
var dem=ee.Image('CGIAR/SRTM90_V4')//GME/images/04040405428907908306-08319720230328335274')//
dem=dem.mask()//  Extract the mask which is essentially land-ocean
var maska=dem.eq(1).clip(studyArea).focal_max(500,"circle","meters").eq(1).select([0],["MASK"])
//var mask=maska
//addToMap(maska,{},"LandMask")

/////////////////////////////////////////////////////
/// Functions & housekeeping

// identify start and stop years
var yearstart=datestart.getUTCFullYear();
var yearstop=datestop.getUTCFullYear();

// get array of years to process

//function to return year+1
var yearplus=function (x, y) { return yearstart +y ; }

var years=Array.apply(0, Array(yearstop-yearstart+1)).map(yearplus);
var nYears=years.length;
if(verbose){print('Processing '+years)}

// function to apply mask to all images in collection
function fprocess(img) {
  return(img.select("greenness").multiply(100).mask(maska).int8());
}

// filter by time
var tfilter=ee.Filter.calendarRange(yearstart,yearstop,'year')

var NDVI_PALETTE = 
    'FFFFFF,CE7E45,DF923D,F1B555,FCD163,99B718,74A901,66A000,529400,' +
    '3E8601,207401,056201,004C00,023B01,012E01,011D01,011301';

//////////////////////////////////////////////////////////
// now get to work...


// loop over products
for (var i=0; i<prods.length; i ++) {
      print(prods[i])
      var tname=prods[i].replace("LANDSAT/", "") //extract make product name
      
    var ndvi = ee.ImageCollection(prods[i]).filter(tfilter).map(fprocess)
    // if there is no images within the date range, skip it...
    if((ndvi.getInfo().features.length)===0) continue;

if(drawmap) {
for(var year = yearstart; year <= yearstop; year+=1) {
addToMap(ndvi,{min:-25,max:100,palette:NDVI_PALETTE},prods[i]+year,1);
}


if(exportfiles){

  var filename=date+'_'+run+'_'+tname+'_'+yearstart+yearstop;
  if(verbose){  print('Exporting to: '+filename)}
 print(ndvi.getInfo())
 exportImage(ndvi,filename,
    {'maxPixels':1000000000,
    'driveFolder':driveFolder,
    'crs': 'EPSG:4326',
    'scale': 30,
    'region': studyArea.geometry().coordinates().getInfo()[0]
  });
  }  
  

}}
