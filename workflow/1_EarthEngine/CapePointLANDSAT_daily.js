// CapePointLANDSAT_daily

//  Specify destination and run name
var drawmap=false;    // flag indicating whether to add the images to the map (below) or not.
var exportfiles=true; // flag indicating whether to actually initiate the export, leve this false while testing

var driveFolder="ee_ZA_output"; // name of personal google drive folder to export to (this must be unique)
var run="full"; // any string to indicate a version.  I typically use a hash from my git repository
var verbose=true;     // print various status messages to the console (on the right)

// limit overall date range  (only dates in this range will be included)
var datestart=new Date("1984-01-01");  // default time zone is UTC
var datestop=new Date("2014-12-31");

// Identify LANDSAT collections to include in processing

// create array of all products to process
var prods =['LANDSAT/LT4_L1T_TOA',
            'LANDSAT/LT5_L1T_TOA',
            'LANDSAT/LE7_L1T_TOA',
            'LANDSAT/LC8_L1T_TOA'
            ] ;

print(prods)
// Get current date as string for file metadata
var currentdate = new Date();
var date= currentdate.getFullYear()+''+("0" + (currentdate.getMonth() + 1)).slice(-2)+''+currentdate.getDate();
if(verbose) print(date);

// zoom to Cape
centerMap(18.4,-34.2,11);

// Define a rectangular region to limit the analysis
var studyArea = ee.Geometry.Polygon([[18.3,-33.89], [18.3,-34.4], [18.5, -34.4],[18.5, -33.89], [18.3,-33.89]]);
var fstudyArea=ee.Feature(studyArea)

// Create a mask using SRTM elevation to mask out water
var dem=ee.Image('CGIAR/SRTM90_V4');
dem=dem.mask();//  Extract the mask which is essentially land-ocean
var mask=dem.eq(1).focal_max(500,"circle","meters").eq(1).select([0],["MASK"]); //.clip(studyArea)

/////////////////////////////////////////////////////
/// Functions & housekeeping

// identify start and stop years
var yearstart=datestart.getUTCFullYear();
var yearstop=datestop.getUTCFullYear();

// function to calculate NDVI, apply cloud mask, and mask to land
// offset by 2 then scale by 1000 to avoid problems with nodata upon export

var cloudthresh=60
var cloudbuff=8
// calculate NDVI and "Simple Landsat Cloud Score" 
var fNDVI = function(img) {
  var score = ee.Algorithms.SimpleLandsatCloudScore(img);
  var ndvi = score.normalizedDifference(['B4', 'B3']).select([0],['ndvi_cloud']);
  var cloud = score.select(['cloud']).lt(cloudthresh).focal_min(cloudbuff).select([0],['cloud']);
  var cloudfree = ndvi.mask(cloud).select([0],['ndvi']);
  return(cloudfree.add(2).multiply(1000).toInt16().where(mask.eq(0),0));
}


      // filter by time
    var tfilter=ee.Filter.calendarRange(yearstart,yearstop,'year')
    var sfilter=ee.Filter.bounds(studyArea);


var NDVI_PALETTE = 
    'FFFFFF,CE7E45,DF923D,F1B555,FCD163,99B718,74A901,66A000,529400,' +
    '3E8601,207401,056201,004C00,023B01,012E01,011D01,011301';

//////////////////////////////////////////////////////////
// now get to work...

prods=['LANDSAT/LE7_L1T_TOA'];

// loop over products (LANDSAT versions)
for (var i=0; i<prods.length; i ++) {
      var tname=prods[i].replace("LANDSAT/", ""); //extract make product name

      // make an image collection, filter by time and space, and run fprocess()
      var col = ee.ImageCollection(prods[i]).filter(tfilter).filter(sfilter);
print(col.getInfo())
      // get list of years in this product
    var  dates=col.aggregate_array('DATE_ACQUIRED').getInfo()
    print(dates)
      print("Found "+dates.length+" Dates for "+tname);
      // if there is no images within the date range, skip it...
      if(dates.length===0) continue;
      
      // loop over years and combine to a single multiband image
      var allndvi = col.map(fNDVI).reduce(ee.Reducer.first())

//        print(ndvi.getInfo())   
        print(allndvi.getInfo())   
        
//      for(var y = 1; y < dates.length; y+=1) {
var y=1
      //    print(ndvi.getInfo().features[y].properties);
      var tdate=new Date(dates[y])
      print(tdate)
          var tfilter2=ee.Filter.metadata('DATE_ACQUIRED','equals',dates[y])
          var tndvi=col.filter(tfilter2).map(fNDVI).reduce(ee.Reducer.first())
          print(tndvi.getInfo())
          var allndvi = allndvi.addBands(tndvi);

          if(drawmap) {
              addToMap(tndvi,{min:1500,max:3000,palette:NDVI_PALETTE},'ndvi',1);
          }
  
        if(exportfiles){
          var filename=date+'_'+run+'_'+tname+'_daily__'+dates[0]+"-"+dates[dates.length-1];
//          print(filename)
          if(verbose){  print('Exporting to: '+filename)} 
           exportImage(allndvi,filename,
                {'maxPixels':1000000000,
                'driveFolder':driveFolder,
                  'crs': 'EPSG:32734',
                  'crs_transform': '[30,0,249990,0,-30,6247260]',
                  'dimensions': '[674,1929]'
                });
  }  

}
//}