import React from 'react'
import PropTypes from 'prop-types'

import Button from '../misc/button'

const inputId = 'dataset_file'

const handleFileInternal = handleFile => () => {
    const file = document.getElementById(inputId).files[0]

    const fr = new FileReader()

    fr.onload = (e) => {
        const setname = file.name.match(/(.*).json$/)[1]
        const dataset = JSON.parse(e.target.result)

        handleFile(dataset, setname)
    }
    fr.readAsText(file)
}

const UploadButton = props => (
    <div>
        <input
            type="file"
            onChange={handleFileInternal(props.uploadDataset)}
            id={inputId}
            style={{ display: 'none' }}
        />
        <Button
            label="Upload Dataset"
            action={() => document.getElementById(inputId).click()}
        />
    </div>
)

UploadButton.propTypes = {
    uploadDataset: PropTypes.func.isRequired,
}

export default UploadButton
