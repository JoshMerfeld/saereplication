// NOTE: I first uploaded this shapefile to GEE. 
var table = ee.FeatureCollection("projects/ee-geefolder/assets/benin_admin2");

// simplify the geometry (this doesn't matter for benin, but for larger areas this can be useful)
var simplified = table.map(function(feature) {
  return feature
});


Map.centerObject(simplified);
Map.addLayer(simplified);


// Function to extract image values for each polygon
var extractValues = function(image) {
  // Reduce image to the polygons in the shapefile
  var extracted = image.reduceRegions({
    collection: simplified,
    reducer: ee.Reducer.mean()
  });

  // Add an 'imageId' property to each feature
  extracted = extracted.map(function(feature) {
    return feature.set('imageId', image.id());
  });

  return extracted;
};


// precip/climate
var dataset = ee.ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")
                  .filterDate('2001-01-01', '2018-12-31')
                  // just NDVI
                  .select(['pr', 'pdsi', 'soil', 'tmmn', 'tmmx'])
                  .filterBounds(table);

// Map over the image collection and extract values for each image
var extractedValues = dataset.map(extractValues).flatten();

// Remove the geometry column from the extracted values
extractedValues = extractedValues.map(function(feature) {
  return feature.select(extractedValues.first().propertyNames().remove('.geo'));
});

// Export the extracted values as a CSV file to your Google Drive or download it locally
Export.table.toDrive({
  collection: extractedValues,
  description: 'terraclimate',
  folder: 'unsae',
  fileFormat: 'CSV'
});
