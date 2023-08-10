// NOTE: I first uploaded this shapefile to GEE. 
var table = ee.FeatureCollection("projects/ee-geefolder/assets/benin_admin2");

// simplify the geometry (this doesn't matter for benin, but for larger areas this can be useful)
var simplified = table.map(function(feature) {
  return feature
});


Map.centerObject(simplified);
Map.addLayer(simplified);


// land class
var dataset = ee.ImageCollection("MODIS/061/MCD12Q1")
                  .filterDate('2018-01-01', '2018-12-31')
                  .select(['LC_Type1'])
                  .filterBounds(simplified);

// Create a function to process each image
var processImage = function(image) {
  var landCover = image.select('LC_Type1');
  var landCoverProportionsImage = landCover.reduceRegions({
    collection: simplified,
    reducer: ee.Reducer.frequencyHistogram().unweighted()
  });
  
  // Get the image date and add it as a property to each feature
  var imageDate = image.date();
  landCoverProportionsImage = landCoverProportionsImage.map(function(feature) {
    return feature.set('imageDate', imageDate);
  });
  
  return landCoverProportionsImage;
};

// Apply the function to each image in the ImageCollection
var landCoverProportions = dataset.map(processImage).flatten();

// Export the extracted values as a CSV file to your Google Drive or download it locally
Export.table.toDrive({
  collection: landCoverProportions,
  description: 'lc',
  folder: 'unsae',
  fileFormat: 'CSV'
});













