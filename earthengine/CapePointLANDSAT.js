// CapePointLANDSAT 


//  Specify destination and run name
var driveFolder="ee_ZA_output";
var run="b2599552"


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

function fmask(img) {
  return(img.mask(maska));
}

//Subset by region and time
var startdate='2000-01-01';
var stopdate='2010-02-28';
var L7 = ee.ImageCollection('LANDSAT/LE7_L1T_ANNUAL_GREENEST_TOA').select("greenness")
var L5= ee.ImageCollection('LANDSAT/LT5_L1T_ANNUAL_GREENEST_TOA').select("greenness")
var L4 = ee.ImageCollection('LANDSAT/LT4_L1T_ANNUAL_GREENEST_TOA').select("greenness")

//var ndvi= LTOA.flatten()//select("greenness")//

//print(ndvi.getInfo());


var NDVI_PALETTE = 
    'FFFFFF,CE7E45,DF923D,F1B555,FCD163,99B718,74A901,66A000,529400,' +
    '3E8601,207401,056201,004C00,023B01,012E01,011D01,011301';

var year=1982;
for(year = 2004; year <= 2009; year+=1) {
var N7=L7.filter(ee.Filter.calendarRange(year,year,'year')).map(fmask)
addToMap(N7,{min:-1,max:1,palette:NDVI_PALETTE},'L7_'+year,1);

var N5=L5.filter(ee.Filter.calendarRange(year,year,'year'))
addToMap(N5,{min:-1,max:1,palette:NDVI_PALETTE},'L5_'+year,1);


}


if(export){
  var filename=date+'_'+run+'_'+yearstart+yearstop+'_'+mcol+'_'+tmonth;
  if(verbose){  print('Exporting to: '+filename)}

 exportImage(N7,tfilename,
    {'maxPixels':1000000000,
    'driveFolder':driveFolder,
    'crs': 'SR-ORG:6974', //4326
    'scale': '926.625433055833',
    'region': region
  });
  }  
  
  
