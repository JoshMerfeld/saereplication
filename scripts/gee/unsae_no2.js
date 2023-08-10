// NOTE: I first uploaded this shapefile to GEE. 
var table = ee.FeatureCollection("projects/ee-geefolder/assets/benin_admin2");

// simplify the geometry (this doesn't matter for benin, but for larger areas this can be useful)
var simplified = table.map(function(feature) {
  return feature
});


// Define the image collection
var collection = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2");

// Create an empty feature collection to accumulate the data
var featureCollection = ee.FeatureCollection([]);

// Define the function to extract data for each month and accumulate it in the feature collection
var extractMonthlyData = function(year, month) {
  // Calculate the start and end dates for the month
  var startDate = ee.Date.fromYMD(year, month, 1);
  var endDate = startDate.advance(1, 'month');
  
  // Filter the collection for the current month
  var filteredCollection = collection.filterDate(startDate, endDate);
  
  // Reduce the image collection to a single image using the mean reducer
  var meanImage = filteredCollection.mean();
  
  // Reduce the image to polygons in the shapefile
  var polygons = meanImage.reduceRegions({
    collection: simplified,
    reducer: ee.Reducer.mean(),
    scale: 2000 // Set the scale to match the data resolution
  });
  
  // Add the year and month as properties to each polygon feature
  polygons = polygons.map(function(feature) {
    return feature.set({
      'year': year,
      'month': month
    });
  });
  
  // Add the polygons to the feature collection
  featureCollection = featureCollection.merge(polygons);
};

// Loop over the years and months
for (var year = 2019; year <= 2019; year++) {
  for (var month = 1; month <= 12; month++) {
    extractMonthlyData(year, month);
  }
}


// Export the extracted values as a CSV file to your Google Drive or download it locally
Export.table.toDrive({
  collection: featureCollection,
  description: 'pollution_no2',
  folder: 'unsae',
  fileFormat: 'CSV'
});




