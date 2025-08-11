

// Build the Lunr index
const idx = lunr(function () {
    this.ref('catalogueNumber') // <-- This is your custom "ref"

    this.field('workTitle')
    this.field('date')
    this.field('medium')
    this.field('description')
    this.field('location')
    this.field('exhibitions')

    works.forEach(function (doc) {
        doc.catalogueNumber = doc.catalogueNumber.toString() // Ensure string
        this.add(doc)
    }, this)
})

// Listen to input changes
document.getElementById('searchBox').addEventListener('input', function () {
    const query = this.value
    const results = idx.search(query)
    const output = document.getElementById('results')
    output.innerHTML = ''

    results.forEach(result => {
        const work = works.find(w => w.catalogueNumber.toString() === result.ref)
        const matchedFields = Object.keys(result.matchData.metadata)

        const highlight = (field, label) => {
            const val = work[field]
            return matchedFields.includes(field)
                ? `<strong>${label}: ${val}</strong>`
                : `${label}: ${val}`
        }

        const imagePath = `./images/${work.catalogueNumber}.svg`

        const html = `
     <div class="work" style="display: flex; align-items: flex-start; gap: 1em; margin-bottom: 1.5em; border-bottom: 1px solid #ccc; padding-bottom: 1em;">
       <img src="${imagePath}" 
           alt="Image for catalogue ${work.catalogueNumber}" 
           style="max-width: 200px; height: auto;" />
      <div>
        <div>${highlight('workTitle', 'Title')}</div>
        <div>${highlight('date', 'Date')}</div>
        <div>${highlight('medium', 'Medium')}</div>
        <div>${highlight('catalogueNumber', 'Catalogue #')}</div>
        <div>${highlight('location', 'Location')}</div>
        <p>${highlight('description', 'Description')}</p>
      </div>
    </div>
  `
        output.innerHTML += html
    })

    if (query && results.length === 0) {
        output.innerHTML = '<p>No matches found.</p>'
    }
})
