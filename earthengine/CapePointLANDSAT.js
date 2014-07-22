// CapePointLANDSAT 

//  Specify destination and run name
var drawmap=false;    // flag indicating whether to add the images to the map (below) or not.
var exportfiles=true; // flag indicating whether to actually initiate the export, leve this false while testing

var driveFolder="ee_ZA_output"; // name of personal google drive folder to export to (this must be unique)
var run="1b93885d00"; // any string to indicate a version.  I typically use a hash from my git repository
var verbose=true;     // print various status messages to the console (on the right)

// limit overall date range  (only dates in this range will be included)
var datestart=new Date("1978-01-01");  // default time zone is UTC
var datestop=new Date("2014-12-31");

// Identify LANDSAT collections to include in processing

// create array of all products to process
var prods =['LANDSAT/LT4_L1T_ANNUAL_GREENEST_TOA',
            'LANDSAT/LT5_L1T_ANNUAL_GREENEST_TOA',
            'LANDSAT/LE7_L1T_ANNUAL_GREENEST_TOA',
            'LANDSAT/LC8_L1T_ANNUAL_GREENEST_TOA'
            ] ;


// Get current date as string for file metadata
var currentdate = new Date();
var date= currentdate.getFullYear()+''+("0" + (currentdate.getMonth() + 1)).slice(-2)+''+currentdate.getDate();
if(verbose) print(date);

// zoom to Cape
centerMap(18.4,-34.2,11);

// Define a rectangular region to limit the analysis
var studyArea = ee.Feature(ee.Geometry.Polygon([[18.3,-33.89], [18.3,-34.4], [18.5, -34.4],[18.5, -33.89], [18.3,-33.89]]));

// Create a mask using SRTM elevation to mask out water
var dem=ee.Image('CGIAR/SRTM90_V4');
dem=dem.mask();//  Extract the mask which is essentially land-ocean
var mask=dem.eq(1).clip(studyArea).focal_max(500,"circle","meters").eq(1).select([0],["MASK"]);

/////////////////////////////////////////////////////
/// Functions & housekeeping

// identify start and stop years
var yearstart=datestart.getUTCFullYear();
var yearstop=datestop.getUTCFullYear();

// get array of years to process

//function to return year+1
var yearplus=function (x, y) { return yearstart +y ; };

var years=Array.apply(0, Array(yearstop-yearstart+1)).map(yearplus);
var nYears=years.length;
if(verbose){print('Processing '+years)}

// function to apply mask to all images in collection
function fprocess(img) {
  return(img.select("greenness").multiply(100).int16().where(mask.eq(0),-32768))
}

      // filter by time
    var tfilter=ee.Filter.calendarRange(yearstart,yearstop,'year');


var NDVI_PALETTE = 
    'FFFFFF,CE7E45,DF923D,F1B555,FCD163,99B718,74A901,66A000,529400,' +
    '3E8601,207401,056201,004C00,023B01,012E01,011D01,011301';

//////////////////////////////////////////////////////////
// now get to work...

//prods=prods[3];

// loop over products (LANDSAT versions)
for (var i=0; i<prods.length; i ++) {
//      print(prods[i]);
      var tname=prods[i].replace("LANDSAT/", ""); //extract make product name

      // make an image collection, filter it to this year, and run fprocess()
      var ndvi = ee.ImageCollection(prods[i]).filter(tfilter)

      // get list of years in this product
      years=ndvi.aggregate_array('system:index').getInfo()
      print("Found "+years.length+" Years for "+tname);

      // if there is no images within the date range, skip it...
      if(years.length===0) continue;
      
      // loop over years and combine to a single multiband image
      var allndvi = ndvi.filter(ee.Filter.calendarRange(parseInt(years[0]),parseInt(years[0]),'year')).
      map(fprocess).reduce(ee.Reducer.first()).select([0],[years[0]]);
      
//        print(ndvi.getInfo())   
//        print(allndvi.getInfo())   
        
      for(var y = 1; y < years.length; y+=1) {
      //    print(ndvi.getInfo().features[y].properties);
          var tfilter2=ee.Filter.calendarRange(parseInt(years[y]),parseInt(years[y]),'year')
          var tndvi=ndvi.filter(tfilter2).map(fprocess).reduce(ee.Reducer.first()).select([0],[years[y]])
//          print(tndvi.getInfo())
          var allndvi = allndvi.addBands(tndvi);

          if(drawmap) {
              addToMap(tndvi,{min:-25,max:100,palette:NDVI_PALETTE},tname+years[y],0);
          }
      }
 
      // now export the multi-band image
//          print(allndvi.getInfo())

      if(exportfiles){
          var filename=date+'_'+run+'_'+tname+'__'+years[1]+"-"+years[years.length-1];
//          print(filename)
          if(verbose){  print('Exporting to: '+filename)} 
           exportImage(allndvi,filename,
                {'maxPixels':1000000000,
                'driveFolder':driveFolder,
                'crs': 'EPSG:32734',
                'scale': 30,
                'region': studyArea.geometry().coordinates().getInfo()[0]
                });
  }  
  
}
