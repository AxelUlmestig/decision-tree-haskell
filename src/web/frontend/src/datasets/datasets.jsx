import React from 'react'
import PropTypes from 'prop-types'

import SectionHeader from '../misc/sectionheader.jsx'
import UploadButton from './uploadbutton.jsx'
import DatasetList from './datasetlist.jsx'

const Datasets = props => (
    <div>
        <SectionHeader value="Datasets" />
        <DatasetList
            datasets={props.datasets}
            deleteDataset={props.deleteDataset}
        />
        <UploadButton uploadDataset={props.uploadDataset} />
    </div>
)

Datasets.propTypes = {
    datasets: PropTypes.arrayOf(PropTypes.shape({
        name: PropTypes.string,
    })).isRequired,
    deleteDataset: PropTypes.func.isRequired,
    uploadDataset: PropTypes.func.isRequired,
}

export default Datasets
