
(function() {
    
    var width = 1024,
    height = 500;

    var data = ["Alcohol use", "Amphetamine use"]

    var svg = d3.select("#chart")
    .append("svg")
    .attr("height", height)
    .attr("width", width)
    
    .append("g")

    .attr("transform", "translate(0,0)")

    

    var radiusScale = d3.scaleSqrt().domain([60,500]).range([10, 30])

    var color = d3.scaleOrdinal(d3.schemeCategory20)
    
    var forceXSep = d3.forceX(function(d){
        if (d.Risk === 'Alcohol'){
            return 180
        } else if (d.Risk === 'Amphetamine') {
            return 330
        } else if (d.Risk === 'Cocaine') {
            return 480
        } else if (d.Risk === 'Illicit drug') {
            return 630
        } else if (d.Risk === 'Partner violence') {
            return 750
        }  else {
            return 900
        }
    }).strength(0.03)

    var forceXComb = d3.forceX(function(d){
        return (width / 2)
    }).strength(0.01)

    var forceCollide = d3.forceCollide(function(d){
        return radiusScale(d.Self/2.1)
    })

    var forceYComb = d3.forceY(function(d){
        return (height / 2)
    }).strength(0.01)
    
    
    // get the bubbles into middle
    var simulation = d3.forceSimulation()
        .force("x", forceXComb)
        .force("y", forceYComb)
        .force("colliding", forceCollide)
        

    d3.queue()
    .defer(d3.csv, "suicide.csv")
    .await(ready)

    


    function ready (error, datapoints){
        var circles = svg.selectAll(".circles")
        .data(datapoints)
        .enter().append("circle")
        .attr("class", "circles")
        .attr("r", function(d){
            return radiusScale(d.Self/2.1)
        })
        
        .attr("fill", function(d) { return color(d.Risk); })
        
        .on('click', function(d){
            console.log(d)
        });

        // circles.append("rect")
        // .attr("width", width / 2)
        // .attr("height", height / 2)
        
        var texts = svg.selectAll(".myTexts")
        .data(datapoints)
        .enter()
        .append("text");

        texts.attr("x", function(d){
            if (d.Risk === 'Alcohol'){
                return 150
            } else if (d.Risk === 'Amphetamine') {
                return 280
            } else if (d.Risk === 'Cocaine') {
                return 450
            } else if (d.Risk === 'Illicit drug') {
                return 590
            } else if (d.Risk === 'Partner violence') {
                return 710
            }  else {
                return 880
            }
        })
            .data(datapoints)
            
            .attr("y", height / 1.2)
            
            .text(function(d) {return d.Risk})
            .attr("transform", "translate(0,0)")
        

        
        
         

    d3.select("#risk").on('click', function(){
        simulation
        .force("x", forceXSep)
        .alphaTarget(0.25)
        .restart()
    })

    d3.select("#combine").on('click', function(){
        simulation
        .force("x", forceXComb)
        .alphaTarget(0.2)
        .restart()
        
    })

    
        
        simulation.nodes(datapoints).on('tick', ticked)

        function ticked() {
            circles
            .attr("cx", function(d){
                return d.x
            })
            .attr("cy", function(d){
                return d.y
            })
        }
    } 
})();