import React from 'react'
import PropTypes from 'prop-types'

import SectionHeader from '../misc/sectionheader'
import UploadButton from './uploadbutton'
import DatasetList from './datasetlist'

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
