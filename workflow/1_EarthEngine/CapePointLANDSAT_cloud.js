// CapePointLANDSAT_cloud

// Illustration of 'simple' cloud masking of LANDSAT data


// Define a rectangular region to search landsat scenes
var studyArea = ee.Geometry.Polygon([[18,-33.89], [18,-34.4], [18.5, -34.4],[18.5, -33.89], [18,-33.89]]);

// Define a LANDSAT collection and filter by date
var collection = ee.ImageCollection('LANDSAT/LE7_L1T_TOA')
        .filterDate(new Date('10/1/1999'), new Date('10/31/1999'))
        .filterBounds(studyArea);

// if desired, print some information about the collection
//print(collection.getInfo())        

var cloudthresh=60 // threshold to use for cloud mask
var cloudbuff=8 // spatial buffer around clouds to remove shadows

// calculate NDVI and "Simple Landsat Cloud Score" 
var NDVI = collection.map(function(img) {
  var score = ee.Algorithms.SimpleLandsatCloudScore(img);
  var ndvi = score.normalizedDifference(['B4', 'B3']).select([0],['ndvi']);
  var cloud = score.select(['cloud']).lt(cloudthresh).focal_min(cloudbuff).select([0],['cloud']);
  var cloudfree = ndvi.mask(cloud).select([0],['cndvi']);
  return(score.addBands(ndvi).addBands(cloudfree));
});



// zoom to Cape
centerMap(18.4,-34.2,11);

// define a color palette for NDVI
var NDVI_PALETTE = 
    'FFFFFF,CE7E45,DF923D,F1B555,FCD163,99B718,74A901,66A000,529400,' +
    '3E8601,207401,056201,004C00,023B01,012E01,011D01,011301';

// Add the images to the map
addToMap(NDVI,{'bands': ['B3', 'B2', 'B1'],min:0,max:.3 },"RGB")
addToMap(NDVI.select(['cloud']),{min:0,max:100,palette:'000000,FF0000'},'cloudscore',1);
addToMap(NDVI.select(['ndvi']),{min:0, max:1,palette:NDVI_PALETTE},'ndvi',1);
addToMap(NDVI.select(['cndvi']),{min:0, max:1,palette:NDVI_PALETTE},'cndvi',1);
